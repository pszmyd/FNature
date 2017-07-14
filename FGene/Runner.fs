namespace FGene

open Nessos.Streams

open FGene.Global
open FGene.Global.Random
open FGene.Population
open FGene.Chromosomes
open FGene.Global.Helpers
open FGene.Global.Operators
open FGene.Selection
open FGene.Selection.Algorithms

type public Runtime<'T when 'T : comparison>(fitness : IChromosome<'T> -> float) = 

    let mutable _algorithm = lazy(Random)
    let mutable _population = lazy(Population<'T>(100, 10))
    let mutable _stop = fun (epoch : int) (fitness : float) -> false
    let mutable _mutationRate = 0.0
    let mutable _crossoverRate = 1.0
    let mutable _maxMutationRate = 0.0
    let mutable _isAdaptiveMutation = false;

    /// Selects an algorithm used for selecting chromosomes for the next population
    member this.WithAlgorithm (alg : Algorithm) = 
        _algorithm <- lazy(alg)
        this
    
    member this.WithPopulation pop = 
        _population <- lazy(pop)
        this

    member this.StopWhenFitness fitness = 
        _stop <- fun epoch f -> f >= fitness
        this

    member this.StopAfter times = 
        _stop <- fun epoch f -> epoch >= times
        this

    member this.StopWhenUnchanged times = 
        _stop <- fun epoch f -> epoch >= times
        this

    member this.WithAdaptiveMutation (min : float) (max : float) =
        _mutationRate <- min
        _maxMutationRate <- max
        _isAdaptiveMutation <- true
        this 

    member this.WithMutation (value : float) =
        _mutationRate <- value
        _isAdaptiveMutation <- false
        this 

    member this.WithCrossover (value : float) =
        _crossoverRate <- value
        this 
        

    member public this.Run() = 
        let pop = _population.Value

        pop.Chromosomes <- this.Fit(pop.Chromosomes |> Stream.ofSeq) |> Stream.toSeq |> Seq.toList

        let mutable best = (pop.Chromosomes |> Seq.maxBy(fun x -> x.Fitness))
        let mutable bestFit = best.Fitness
        let mutable previousBestFits = [ 0.0 ]
        let mutable bestText = best.ToString()
        let diff = _maxMutationRate - _mutationRate

        let mutable i = 0;

        while not(_stop i bestFit) do
            let chosen = this.Select(pop)
                          |> Stream.sortBy(fun _ -> rng.NextDouble())
                          |> Stream.pair
                          |> Stream.map (fun (ch1, ch2) -> _crossoverRate |> (ch1 <||> ch2))
                          |> Stream.collect (fun x -> seq { yield fst x 
                                                            yield snd x } |> Stream.ofSeq)
                          |> Stream.map(fun x -> x <~~ match _isAdaptiveMutation with
                                                          | true -> _mutationRate + (diff * sqrt((previousBestFits |> average) / (previousBestFits |> List.max)))
                                                          | false -> _mutationRate)
                          |> this.Fit

            pop.Chromosomes <- chosen |> Stream.toSeq |> Seq.toList

            let newBest = pop.Chromosomes |> Seq.maxBy(fun x -> x.Fitness)
            previousBestFits <- if i > 1000 then (bestFit :: previousBestFits.[0..999]) else bestFit :: previousBestFits

            if newBest.Fitness > bestFit then
                best <- newBest
                bestFit <- newBest.Fitness
                bestText <- newBest.ToString()

            if i % 100 = 0 then
                printfn "%s.\t%s" (i.ToString()) (bestText)

            i <- i + 1

        printfn "%s.\t%s" (i.ToString()) (bestText)
        pop

    member private this.Fit(chromosomes : Stream<IChromosome<'T>>) = 
        chromosomes |> Stream.map(fun c -> c.Fitness <- fitness(c); c)

    member private this.Select(pop : Population<'T>) = 
        pop |> apply <| _algorithm.Value


module Runner =
    let Create<'T when 'T : comparison> fitness = Runtime<'T>(fitness)

