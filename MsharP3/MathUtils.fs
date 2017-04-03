namespace MsharP3

open Sideinfo
open Frame
open ScaleFactors
open SineTables
open SynthTables

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
        let sqrt2 = 1.414213562373095
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
        let output = Array.zeroCreate 576

        let test width = 
            for ss = 0 to width do
                output.[start + block + 0] <- samples.[total + ss + width * 0]
                output.[start + block + 6] <- samples.[total + ss + width * 1]
                output.[start + block + 12] <- samples.[total + ss + width * 2]
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

        output
    
    //Alias reduction
    let reduceAlias (granule:sideInfoGranule) (samples:float []) = 
        let output =
            if samples.Length < 576
                then failwith "Array length less than 576"
                else samples
        
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
                let s1 = output.[offset1]
                let s2 = output.[offset2]
                output.[offset1] <- (s1 * cs.[sample]) - (s2 * ca.[sample])
                output.[offset2] <- (s2 * cs.[sample]) + (s1 * ca.[sample])
        output
    
    //Inverse Modified Discrete Cosine Transform
    let prevSamples = Array3D.create 2 32 18 0.0
    let IMDCT (granule:sideInfoGranule) (samples:float []) = 
        
        let PI = 3.141592653589793
        let N = if granule.blockType = 2 then 12 else 36
        let halfN = N/2
        let output = samples
        let winMax = if granule.blockType = 2 then 2 else 0
        let sampleBlock = Array.create 36 0.0
        let mutable sample = 0

        for block = 0 to 31 do
            for win = 0 to winMax do
                for i = 0 to (N - 1) do
                    let mutable xi = 0.0
                    for k = 0 to (halfN - 1) do
                        let s = output.[18 * block + halfN * win + k];
                        xi <- xi + s * cos((PI / ((2 * N) |> float)) * ((2 * i + 1 + halfN) |> float) * ((2 * k + 1) |> float));
                    sampleBlock.[win * N + i] <- xi * sineblock.[granule.blockType].[i]

            if granule.blockType = 2 then
                let tempBlock = sampleBlock
                for i = 0 to 5 do
                    sampleBlock.[i] <- 0.0
                for i = 6 to 11 do
                    sampleBlock.[i] <- tempBlock.[0 + i - 6]
                for i = 12 to 17 do
                    sampleBlock.[i] <- tempBlock.[0 + i - 6] + tempBlock.[12 + i - 12]
                for i = 18 to 23 do
                    sampleBlock.[i] <- tempBlock.[12 + i - 12] + tempBlock.[24 + i - 18]
                for i = 24 to 29 do
                    sampleBlock.[i] <- tempBlock.[24 + i - 18]
                for i = 30 to 35 do
                    sampleBlock.[i] <- 0.0
             
            for i = 0 to 17 do
                output.[sample + i] <- sampleBlock.[i] + prevSamples.[granule.channel,block,i]
                prevSamples.[granule.channel,block,i] <- sampleBlock.[18 + i]

            sample <- sample + 18

        let mutable sb = 1
        let mutable i = 1
        while sb < 18 do
            while i < 32 do
                output.[i * 18 + sb] <- output.[i * 18 + sb] * -1.0;
                i <- i + 2
            i <- 1
            sb <- sb + 2
        output
    
    let fifo = Array2D.create 2 1024 0.0 //For storing filterbank data
    let synthFilter (granule:sideInfoGranule) (samples:float []) = 
        
        let S = Array.create 032 0.0
        let U = Array.create 512 0.0
        let W = Array.create 512 0.0   
        let channel = granule.channel

        let pcm = Array.create 576 0.0

        for sb = 0 to 17 do
            for i = 0 to 31 do
                S.[i] <- samples.[i * 18 + sb]
                
            for i = 1023 downto 64 do
                fifo.[channel,i] <- fifo.[channel,i - 64]
            
            for i = 0 to 63 do
                fifo.[channel,i] <- 0.0
                for j = 0 to 31 do
                    fifo.[channel,i] <- fifo.[channel,i] + (S.[j] * lookup.[i].[j])

            for i = 0 to 7 do
                for j = 0 to 31 do
                    U.[i * 64 + j] <- fifo.[channel,i * 128 + j]
                    U.[i * 64 + j + 32] <- fifo.[channel,i * 128 + j + 96]

            for i = 0 to 511 do
                W.[i] <- U.[i] * synthWindow.[i]

            for i = 0 to 31 do
                let mutable sum = 0.0
                for j = 0 to 15 do
                    sum <- sum + W.[j * 32 + i];
                pcm.[32 * sb + i] <- sum
        pcm |> Array.map float32

    let interleaveSamples (samples:float32 [][]) = 
        let mutable i1 = -1
        let mutable i2 = -1
        let result = 
            match samples.Length < 2 with
            |true -> failwith "Samples from 2 channels required"
            |false -> 
                [|0..1151|] 
                |> Array.map (fun x -> if (x &&& 0x01) = 0 then i1 <- i1 + 1;samples.[0].[i1] else i2 <- i2 + 1;samples.[1].[i2])
        result
                
