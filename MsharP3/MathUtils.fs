namespace MsharP3

open Sideinfo
open Frame
open ScaleFactors

module MathUtils = 
    let requantizeSamples (sideConfig:sideInfoGranule) (frameinfo:FrameInfo) (scalefactors:ScaleFactors) (samples:int []) = 
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