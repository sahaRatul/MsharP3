namespace MsharP3

open Sideinfo
open Frame
open ScaleFactors
open SineTables

module MathUtils = 
    
    //Requantization
    let requantizeSamples (frameinfo:FrameInfo) (sideConfig:sideInfoGranule) (scalefactors:ScaleFactors) (samples:int []) = 
        let pretab = [|0;0;0;0;0;0;0;0;0;0;0;1;1;1;1;2;2;3;3;3;2;0;0|]
        let mutable sfb = 0
        let mutable i = 0
        let mutable window = 0
        let out = Array.create 576 0.0
        let scalefacMult = if sideConfig.scaleFactorScale = 0 then 0.5 else 1.0

        //Generate exponent for each sample index
        let getExponents sampleindex = 
            let (exponent1,exponent2) = 
                match sideConfig.blockType = 2 || sideConfig.mixedBlockFlag && sfb >= 8 with
                |true -> 
                    let win = 
                        match i = (frameinfo.bandWidth |> snd).[sfb] with
                        |true ->
                            i <- 0
                            if window = 2
                                then
                                    sfb <- sfb + 1
                                    window <- 0
                                    window
                                else 
                                    window <- window + 1
                                    window
                        |false -> window
                    let shortscale = 
                        match scalefactors with
                        |Short x -> x
                        |Mixed x -> fst x
                        |Long x -> failwith "Scalefactor type mismatch. Expected shortscalefactors"
                    let exp1 = (sideConfig.globalGain - 210 - (8 * sideConfig.subBlockGain.[win])) |> float
                    let exp2 = scalefacMult * (shortscale.data.[win,sfb] |> float)
                    (exp1,exp2)
                |false -> 
                    let long = frameinfo.bandIndex |> fst
                    let x = if (sampleindex = long.[sfb+1]) then 1 else 0
                    sfb <- sfb + x
                    let longscale = 
                        match scalefactors with
                        |Long x -> x
                        |_ -> failwith "Scalefactor type mismatch. Expected longscalefactors"
                    let exp1 = (sideConfig.globalGain - 210) |> float
                    let exp2 = ((longscale.data.[sfb] |> float) * scalefacMult) + ((sideConfig.preflag * pretab.[sfb]) |> float)
                    (exp1,exp2)
            (sampleindex,exponent1,exponent2)
        
        //Requantize samples
        let requantize (index,exp1,exp2) = 
            let sign = if samples.[index] < 0 then -1.0 else 1.0
            let a = (samples.[index] |> (abs >> float)) ** (4.0/3.0) 
            let b = 2.0 ** (exp1/4.0)
            let c = 2.0 ** (-exp2)
            sign * a * b * c
        
        let result = 
            [|0..(samples.Length - 1)|] 
            |> Array.map (getExponents >> requantize)
        result
    
    //Mid side Band (for joint stereo)
    let decodeMidSide (ch1:float []) (ch2:float []) = 
        let sqrt2 = 1.414
        if ch1.Length <> ch2.Length 
            then failwith "Array sizes are not equal"
            else
                let left = Array.map2 (fun x y -> (x + y)/sqrt2) ch1 ch2
                let right = Array.map2 (fun x y -> (x + y)/sqrt2) ch1 ch2
                [|left;right|]
    
    //Reorder samples
    let reorderSamples (frameinfo:FrameInfo) (samples:float []) = 
        let mutable total = 0
        let mutable start = 0
        let mutable block = 0
        let temp = Array.zeroCreate 576

        let test width = 
            for ss = 0 to width do
                temp.[start + block + 0] <- samples.[total + ss + width * 0]
                temp.[start + block + 6] <- samples.[total + ss + width * 1]
                temp.[start + block + 12] <- samples.[total + ss + width * 2]
                if (block <> 0 && block % 5 = 0) 
                    then
                        start <- start + 18
                        block <- 0
                    else
                        block <- block + 1

        [|0..11|] 
        |> Array.map (fun x -> (snd frameinfo.bandWidth).[x] - 1)
        |> Array.map test
        |> ignore

        temp
    
    //Alias reduction
    let reduceAlias (granule:sideInfoGranule) (samples:float []) = 
        let temp =
            if samples.Length < 576
                then failwith "Array length less than 576"
                else Array.map (fun x -> x) samples
        
        let cs = [|
            0.8574929257;0.8817419973;0.9496286491;0.9833145925;
            0.9955178161;0.9991605582;0.9998991952;0.9999931551
        |]

        let ca = [|
            -0.5144957554;-0.4717319686;-0.3133774542;-0.1819131996;
            -0.0945741925;-0.0409655829;-0.0141985686;-0.0036999747
        |]

        let sbMax = if granule.mixedBlockFlag then 1 else 31
        
        for sb = 1 to sbMax do
            for sample = 0 to 7 do
                let offset1 = 18 * sb - sample - 1
                let offset2 = 18 * sb + sample
                let s1 = samples.[offset1]
                let s2 = samples.[offset2]
                temp.[offset1] <- s1 * cs.[sample] - s2 * ca.[sample]
                temp.[offset2] <- s2 * cs.[sample] + s1 * ca.[sample]
        temp
