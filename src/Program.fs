open System

open Collections

let readDir dir =
    IO.Directory.GetFiles(dir, "viewtopic.php*")
    |> Array.iter Parsers.Topic.Parse

[<EntryPoint>]
let main argv =
    if argv.Length <> 1 then
        printfn "Usage: %s datadir" System.AppDomain.CurrentDomain.FriendlyName
        1
    else
        printfn "Reading data directory \"%s\"..." argv.[0]
        readDir argv.[0]

        Users.Print()
        Forums.Print()
        Topics.Print ()
        Posts.Print ()

        0
