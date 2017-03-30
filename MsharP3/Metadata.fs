namespace MsharP3

open Utils

module ID3 = 
    type ID3v2Header = {
        identifier:string
        version:List<byte>
        flags:List<byte>
        size:int
    }

    type ID3v2ExtendedHeader = {
        isPresent:bool
        size:int
        flags:List<byte>
        paddingSize:int
    }

    type frameCRC = {
        isPresent:bool
        CRC:List<byte>
    }

    type ID3v2FrameHeader = {
        frameID:string
        size:int
        flags:List<byte>
    }

    type ID3v2tags = {
        header:ID3v2Header
        extendedHeader:ID3v2ExtendedHeader
        CRC:frameCRC
        frames:List<ID3v2FrameHeader>
    }

    let parseID3v2Header (data:array<byte>) = 
        if data.Length < 10 
            then failwith "header data cannot be less than 10 bytes"
            else 
                let Header:ID3v2Header = {
                    identifier = "ID3"
                    version = [data.[4];data.[5]]
                    flags = 
                        let unsync = (data.[5] &&& 0b10000000uy) >>> 7
                        let extended = (data.[5] &&& 0b01000000uy) >>> 6
                        let experimental = (data.[5] &&& 0b00100000uy) >>> 5
                        [unsync;extended;experimental]
                    size = 
                        [data.[9];data.[8];data.[7];data.[6]] //Assuming Little Endian machine
                        |> List.map (((&&&) 0b01111111uy) >> int)
                        |> List.zip [3..0]
                        |> List.map (fun (shift,x) -> x <<< 7 * shift)
                        |> List.fold (fun acc x -> acc ||| x) 0
                }
                (Header,data.[10..Header.size])
    
    let parseID3v2ExtendedHeader ((header:ID3v2Header),data:array<byte>) = 
        if header.flags.[1] = 0uy then
            let (extended:ID3v2ExtendedHeader) = {
                isPresent = false
                size = 0
                flags = [0uy]
                paddingSize = 0
            }
            (extended,data)
            else
                let (extended:ID3v2ExtendedHeader) = {
                    isPresent = false
                    size = 
                        [data.[3];data.[2];data.[1];data.[0]] //Assuming Little Endian machine
                        |> List.map int
                        |> List.zip [3..0]
                        |> List.map (fun (shift,x) -> x >>> 8 * shift)
                        |> List.fold (fun acc x -> acc ||| x) 0
                    flags = [data.[4] >>> 7]
                    paddingSize = 
                        [data.[3];data.[2];data.[1];data.[0]] //Assuming Little Endian machine
                        |> List.map int
                        |> List.zip [3..0]
                        |> List.map (fun (shift,x) -> x >>> 8 * shift)
                        |> List.fold (fun acc x -> acc ||| x) 0
                }
                (extended,data.[10..])

        
