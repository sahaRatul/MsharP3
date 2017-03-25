namespace FSharpMP3

open System
open Header
open Sideinfo
open Maindata

module Test = 
    //Test Data
    let main = 
        let data = IO.File.ReadAllBytes "aframe.mp3"
        let header = data.[0..3] |> parseHeader
        let sideconfig = header |> parseSideConfig data.[4..35]
        sideconfig |> printfn "%A"
        parseMainData data.[36..] header sideconfig |> printfn "%A"