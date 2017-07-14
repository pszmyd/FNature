// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
namespace FGene

open FGene.Global
open FGene.Global.Operators
open FGene.Population

module Main = 
    open System

    [<EntryPoint>]
    let main argv = 
        let target = "HelloWorldLoremIpsumDolorSitAmetHelloWorldLoremIpsumDolorSitAmet"
        let runner = 
            Runner.Create<char> (fun(ch) -> (ch.Contents
                                             |> Seq.mapi(fun i g -> if target.[i] = g.Contents then 
                                                                        match i with
                                                                        | x when x = target.Length - 1 -> if target.[i-1] = ch.Contents.[i-1].Contents then 1.5 else 1.0
                                                                        | x when x = 0 -> if target.[i+1] = ch.Contents.[i+1].Contents then 1.5 else 1.0
                                                                        | x -> if target.[i+1] = ch.Contents.[i+1].Contents && target.[i-1] = ch.Contents.[i-1].Contents then 
                                                                                  2.0 
                                                                               else 
                                                                                  if target.[i+1] = ch.Contents.[i+1].Contents || target.[i-1] = ch.Contents.[i-1].Contents then
                                                                                      1.5
                                                                                  else
                                                                                      1.0
                                                                    else 
                                                                        0.0)
                                             |> Seq.sum))


        runner
            .WithPopulation(Population<char>(100, target.Length))
            .WithAlgorithm(Selection.Algorithms.StochasticUniform 0)
            .WithMutation(0.001)
            .WithCrossover(0.8)
            .StopWhenFitness(127.0)
            .Run()
            .Chromosomes |> ignore

        System.Console.ReadLine() |> ignore
        0 // return an integer exit code
