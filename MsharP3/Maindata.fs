namespace MsharP3

open Utils
open Header
open Frame
open Sideinfo
open HuffmanTables

module ScaleFactors = 
    type ScaleFactorsLong = {
        granule:int
        channel:int
        data:int []
    }

    type ScaleFactorsShort = {
        granule:int
        channel:int
        data:int [,]
    }

    type ScaleFactors = 
    |Short of ScaleFactorsShort
    |Long of ScaleFactorsLong
    |Mixed of (ScaleFactorsShort * ScaleFactorsLong)

    let parseScaleFactors (x:array<byte>) (sideInfoConfig:SideInfoConfig) (y:sideInfoGranule) = 
        let mutable bitcount = 0
        //Keep this for later use
        let mutable scaleLongGr0Ch1 = Array.zeroCreate 20
        let mutable scaleLongGr0Ch2 = Array.zeroCreate 20

        let slen = [|
            [|0;0|];[|0;1|];[|0;2|];[|0;3|];[|3;0|];[|1;1|];[|1;2|];[|1;3|];
            [|2;1|];[|2;2|];[|2;3|];[|3;1|];[|3;2|];[|3;3|];[|4;2|];[|4;3|];
        |]
        let scaleFactorLengths = [|slen.[y.scaleFactorCompress].[0];slen.[y.scaleFactorCompress].[1]|]
        let (result:ScaleFactors) = 
            match (y.blockType,y.windowSwitchFlag) with
            |(2,true) -> //Mixed/Short scalefactors
                match y.mixedBlockFlag with
                |true -> //Mixed
                    let (longScale:ScaleFactorsLong) = {
                        granule = y.granule
                        channel = y.channel
                        data = 
                            let temp = Array.zeroCreate 8
                            let mutable arr = x
                            for i = 0 to 7 do
                                let (num,tmp) = arr |> getBits scaleFactorLengths.[0]
                                arr <- tmp
                                temp.[i] <- num
                            bitcount <- bitcount + (8 * scaleFactorLengths.[0])
                            temp
                    }
                    let (shortScale:ScaleFactorsShort) = {
                        granule = y.granule
                        channel = y.channel
                        data = 
                            let temp = Array2D.zeroCreate 3 13
                            let mutable arr = x
                            for sfb = 3 to 5 do
                                for win = 0 to 2 do
                                    let (num,tmp) = arr |> getBits scaleFactorLengths.[0]
                                    arr <- tmp
                                    temp.[win,sfb] <- num
                            for sfb = 6 to 11 do
                                for win = 0 to 2 do
                                    let (num,tmp) = arr |> getBits scaleFactorLengths.[1]
                                    arr <- tmp
                                    temp.[win,sfb] <- num
                            bitcount <- bitcount + (9 * scaleFactorLengths.[0])
                            bitcount <- bitcount + (18 * scaleFactorLengths.[1])
                            temp
                    }
                    Mixed (shortScale,longScale)
                |false -> //Short
                    let (shortScale:ScaleFactorsShort) = {
                        granule = y.granule
                        channel = y.channel
                        data = 
                            let temp = Array2D.zeroCreate 3 13
                            let mutable arr = x
                            for sfb = 0 to 5 do
                                for win = 0 to 2 do
                                    let (num,tmp) = arr |> getBits scaleFactorLengths.[0]
                                    arr <- tmp
                                    temp.[win,sfb] <- num
                            for sfb = 6 to 11 do
                                for win = 0 to 2 do
                                    let (num,tmp) = arr |> getBits scaleFactorLengths.[1]
                                    arr <- tmp
                                    temp.[win,sfb] <- num
                            temp
                    }
                    bitcount <- bitcount + (18 * scaleFactorLengths.[0])
                    bitcount <- bitcount + (18 * scaleFactorLengths.[1])
                    Short (shortScale)
            |(_,_) -> //Long scalefactors
                match y.granule = 0 with
                |true -> //Granule 0
                    let (longScale:ScaleFactorsLong) = {
                        granule = y.granule
                        channel = y.channel
                        data = 
                            let temp = Array.zeroCreate 22
                            let mutable arr = x
                            for sfb = 0 to 9 do
                                let (num,tmp) = arr |> getBits scaleFactorLengths.[0]
                                arr <- tmp
                                temp.[sfb] <- num
                            for sfb = 10 to 19 do
                                let (num,tmp) = arr |> getBits scaleFactorLengths.[0]
                                arr <- tmp
                                temp.[sfb] <- num
                            if y.channel = 0 
                                then scaleLongGr0Ch1 <- temp
                                else scaleLongGr0Ch2 <- temp
                            temp
                    }
                    bitcount <- bitcount + 20 * scaleFactorLengths.[0]
                    Long (longScale)
                |false -> //Granule 1 (maybe with scalefactor reuse)
                    let (longScale:ScaleFactorsLong) = {
                        granule = y.granule
                        channel = y.channel
                        data = 
                            let temp = Array.zeroCreate 21
                            let mutable arr = x
                            let sb = [|5;10;15;20|]
                            let mutable index = 0
                            for i = 0 to 1 do
                                for sfb = index to sb.[i] do
                                    match sideInfoConfig.scfsi.[y.channel].[i] = 1 with
                                    |true -> 
                                        temp.[sfb] <- if y.channel = 0 
                                                        then scaleLongGr0Ch1.[sfb] 
                                                        else scaleLongGr0Ch2.[sfb]
                                    |false -> 
                                        let (num,tmp) = arr |> getBits scaleFactorLengths.[0]
                                        arr <- tmp
                                        bitcount <- bitcount + (1 * scaleFactorLengths.[0])
                                        temp.[sfb] <- num
                                    index <- sfb
                                index <- index + 1
                            
                            for i = 2 to 3 do
                                for sfb = index to sb.[i] do
                                    match sideInfoConfig.scfsi.[y.channel].[i] = 1 with
                                    |true -> 
                                        temp.[sfb] <- if y.channel = 0 
                                                        then scaleLongGr0Ch1.[sfb] 
                                                        else scaleLongGr0Ch2.[sfb]
                                    |false -> 
                                        let (num,tmp) = arr |> getBits scaleFactorLengths.[1]
                                        arr <- tmp
                                        bitcount <- bitcount + (1 * scaleFactorLengths.[1])
                                        temp.[sfb] <- num
                                    index <- sfb
                                index <- index + 1
                            temp
                    }
                    Long (longScale)
        (result,bitcount)           

module Huffman = 
    let parseHuffmanData (data:array<byte>) maxbit (frame:FrameInfo) (granule:sideInfoGranule) = 
        
        let samples = Array.zeroCreate 576
        let mutable bitsArray = data
        let mutable bitcount = 0
        let mutable samplecount = 0

        let (region0,region1) = //Get boundaries of sample regions
            match(granule.blockType,granule.windowSwitchFlag) with
            |(2,true) -> (36,576)
            |_ -> 
                let reg0 = (frame.bandIndex |> fst).[granule.region0Count + 1]
                let reg1 = (frame.bandIndex |> fst).[granule.region0Count + granule.region1Count + 2]
                (reg0,reg1)
        
        let getTable x = //x = sample number
            match x < region0 with
            |true -> ((getHuffmanTable granule.tableSelect.[0]),(granule.tableSelect.[0]))
            |false ->
                match x < region1 with
                |true -> ((getHuffmanTable granule.tableSelect.[1]),(granule.tableSelect.[1]))
                |false -> ((getHuffmanTable granule.tableSelect.[2]),(granule.tableSelect.[2]))

        let decodeTable x = //x = huffman table
            match (snd x) with
            |0 -> (0,0,0,0) //Sample = 0 for table0
            |_ -> 
                let rec checkInTable table rowindex = 
                    match table with
                    |[] -> (0,0,0,snd x)
                    |head::tail -> 
                        if List.exists (fun (value,size) -> (value = ((getBits32 size bitsArray) |> int))) head
                            then 
                                (
                                    //Size
                                    head.[List.findIndex (fun (value,size) -> (value = ((getBits32 size bitsArray) |> int))) head] |> snd,
                                    //Row
                                    rowindex,
                                    //Col
                                    List.findIndex (fun (value,size) -> (value = ((getBits32 size bitsArray) |> int))) head,
                                    //Table number
                                    snd x
                                )
                            else
                                checkInTable tail (rowindex + 1)

                let (size,row,col,num) = checkInTable (fst x) 0
                bitcount <- bitcount + size
                if size <> 0 then bitsArray <- bitsArray.[size..]
                (size,row,col,num)
        
        let getSample x = //x = output from decodeTable
            let (size,row,col,num) = x
            match (size,row,col) with
            |(0,0,0) -> [0;0]
            |(_,_,_) ->
                let extendSample y value = //y = table num
                    let linbit = 
                        match bigValueLinbit.[num] <> 0, value = (bigValueMax.[num] - 1) with
                        |(true,true) -> 
                            let (bits,temp) = getBits bigValueLinbit.[num] bitsArray
                            bitcount <- bitcount + bigValueLinbit.[num]
                            bitsArray <- temp
                            bits
                        |(_,_) -> 0
                    let sign = 
                        if value > 0 
                            then 
                                let (bits,temp) = getBits 1 bitsArray
                                bitcount <- bitcount + 1
                                bitsArray <- temp
                                if bits = 1 then -1 else 1
                            else
                                1
                    sign * (value + linbit)
                [row;col] |> List.map (extendSample num)
        
        //QuadTables
        let rec getQuadValues x = 
            match((bitcount < maxbit) && ((samplecount + 4) < 576)) with
            |false -> []
            |true -> 
                let quadvalues = 
                    match granule.count1TableSelect = 1 with
                    |true -> //Get 4 bits and flip them
                        let bits = Array.toList bitsArray.[0..3]
                        bitsArray <- bitsArray.[3..]
                        bitcount <- bitcount + 4
                        bits |> List.map (fun x -> if x = 0uy then 1 else 0)
                    |false -> 
                        let rec getvalues x = 
                            match x with
                            |[] -> (0,[0;0;0;0])
                            |((hcode,size),value)::tail -> 
                                if hcode = ((getBits32 size bitsArray) |> int)
                                    then (size,value)
                                    else getvalues tail
                        let (size,values) = quadTable |> getvalues
                        bitsArray <- bitsArray.[size..]
                        bitcount <- bitcount + size
                        values
                let signs = 
                    let bits = Array.toList bitsArray.[0..3]
                    bitsArray <- bitsArray.[3..]
                    bitcount <- bitcount + 4
                    bits
                let result = 
                    signs 
                    |> List.map int
                    |> List.zip quadvalues 
                    |> List.map (fun (x,y) -> (if y = 1 then -x else x))
                samplecount <- samplecount + 4
                result @ getQuadValues (x + 4)


        //Decode huffman tables
        let limit = granule.bigValues * 2
        while samplecount < (limit) do
            let result = samplecount |> (getTable >> decodeTable >> getSample)
            samples.[samplecount] <- result.[0]
            samples.[samplecount + 1] <- result.[1]
            samplecount <- samplecount + 2
        
        //Decode Quad Values table if applicable
        let temp = samplecount
        let quadSamples = getQuadValues 0
        match (temp < samplecount) with
        |false -> 
            samples
        |true ->
            for index = 0 to (samplecount - temp - 1) do
                samples.[temp + index] <- quadSamples.[index]
            samples

module Maindata = 
    
    open ScaleFactors
    open Huffman
    
    //Parse Main data from frame
    let parseMainData (data:array<byte>) (header:HeaderConfig) (frameinfo:FrameInfo) (sideinfo:SideInfoConfig) = 
        let arrayBits = data |> Array.map int |> getBitsArrayfromByteArray
        let mutable bitcount = 0
        let channels = if header.channelMode = 3uy then 1 else 2

        //Create Arrays to store scalefactors and samples
        let (sclfactors,samples) = 
            match channels with
            |1 -> 
                let (sclfactors:array<ScaleFactors>) = Array.zeroCreate 2
                let (samples:array<array<int>>) = Array.zeroCreate 2
                (sclfactors,samples)
            |_ -> 
                let (sclfactors:array<ScaleFactors>) = Array.zeroCreate 4
                let (samples:array<array<int>>) = Array.zeroCreate 4
                (sclfactors,samples)

        for i = 0 to (if channels = 1 then 1 else 3) do
            let maxbit = bitcount + sideinfo.sideInfoGr.[i].par23Length
            let (x,y) = parseScaleFactors (arrayBits.[bitcount..] |> Array.map byte) sideinfo sideinfo.sideInfoGr.[i]
            sclfactors.[i] <- x
            bitcount <- bitcount + y
            samples.[i] <- parseHuffmanData (arrayBits.[bitcount..] |> Array.map byte) maxbit frameinfo sideinfo.sideInfoGr.[i]
            bitcount <- maxbit
        
        (sclfactors,samples)