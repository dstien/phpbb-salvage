module Util

open System
open System.Text.RegularExpressions
open FSharp.Data

open Types

// Read file into HtmlDocument with timestamp.
let ReadFile (filename : string) =
    let timestamp = IO.File.GetCreationTime(filename)

    // Read entire file and remove all newlines. phpbb have inserted <br/> for every newline in post bodies which FSharp.Data substitutes back to newline.
    let src = IO.File.ReadAllText(filename).Replace("\n", "")
    let doc = HtmlDocument.Load(new IO.StringReader(src))

    printfn "Read %s (%s)" filename (timestamp.ToString())

    (doc, timestamp)

// Print items in a sorted dictionary one by one.
let PrintDictionary (dict : Collections.Generic.SortedDictionary<int, 'T>) =
    let mutable em = dict.GetEnumerator()
    while em.MoveNext() do
        printfn "%A" em.Current.Value

// Get a numeric field value from query string.
let NumericQueryField (link : HtmlNode) (field : string) =
    Int32.Parse(Regex.Match(link.AttributeValue("href"), @"(\?|&)" + field + @"=(\d+)").Groups.[2].Value)

// Parse forum timestamp string relative to source time.
let ParseForumTimestamp (text : string) (sourceTime : DateTime) =
    let formatDate (date : DateTime) = date.ToUniversalTime().ToString("dd MMM yyyy")

    let adjustedText =
        match text with
        | t when t.StartsWith("Today") -> text.Replace("Today", formatDate sourceTime)
        | t when t.StartsWith("Yesterday") -> text.Replace("Yesterday", formatDate (sourceTime.AddDays(-1.0)))
        | t -> t

    DateTime.Parse(adjustedText)

// Get the previous source by date, regardless of type.
let PreviousSourceOfAny (sources : Map<SourceType, DateTime>) =
    sources
    |> Map.toList
    |> List.map (fun s -> snd s)
    |> List.sortDescending
    |> List.head

// Get the previous source of given types, default to DateTime.MinValue.
let PreviousSourceOfTypes (sources : Map<SourceType, DateTime>) (types : SourceType list) =
    sources
    |> Map.filter (fun s _ -> types |> List.contains s)
    |> Map.toList
    |> List.map (fun s -> snd s)
    |> List.sortDescending
    |> List.tryHead
    |> Option.defaultValue DateTime.MinValue
