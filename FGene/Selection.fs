namespace FGene.Selection

open FGene.Global
open FGene.Population
open FGene.Chromosomes
open Nessos.Streams

module Algorithms =

    let private sus (number : int) (population : Population<'T>) =    
        let chromosomes = population.Chromosomes
        let numberToPick = if number = 0 then chromosomes.Length else number

        let sumFitness = chromosomes |> Seq.sumBy(fun c -> c.Fitness)
        let stepSize = 1.0 / double numberToPick;

        let accumulativePercent = ref 0.0;
        let rouletteWheel = 
            chromosomes 
            |> List.map(fun c -> accumulativePercent := !accumulativePercent + (c.Fitness  / sumFitness)
                                 !accumulativePercent)
            |> Stream.ofList

        let p = ref(Random.rng.NextDouble())
        [
            for _ in 1..numberToPick ->       
                let pointer = match !p with 
                              | x when x > double 1.0 -> x - double 1.0 
                              | x -> x

                let chromosomeIndex = 
                    rouletteWheel
                    |> Stream.mapi(fun index value -> (value, index))
                    |> Stream.tryFind(fun (value, _) -> value >= pointer)

                p := !p + stepSize;                

                match chromosomeIndex with
                | Some _ -> chromosomes.[snd chromosomeIndex.Value]
                | _ -> chromosomes.[Random.rng.Next(0, chromosomes.Length)]
        ] |> Stream.ofList

    let private randomize (population : Population<'T>) = 
        let arr = population.Chromosomes |> Seq.toArray
        seq { for _ in 1..arr.Length -> arr.[Random.rng.Next(arr.Length)] } |> Stream.ofSeq

    /// List of available selection algorithms
    type public Algorithm = 
        | StochasticUniform of int
        | Random
    
    /// Applies a given algorithm over a given chromosome population, resulting with a new chromosome collection    
    let apply population algorithm = 
        match algorithm with
        | StochasticUniform(number) -> sus number population
        | Random -> randomize population


