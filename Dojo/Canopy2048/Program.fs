(*
The goal of this dojo is to learn the basics of
the F# Canopy web testing framework, written
by Chris Holt (www.twitter.com/lefthandedgoat),
using it to drive the game "2048".

The dojo is setup as as series of tasks,
embedded in this file. It starts with simple
examples using Canopy to interact with the 
game web page, and progressively builds up
to writing an automated bot to play the game.

Have fun! Any feedback is highly appreciated,
either on the GitHub repository itself:
https://github.com/c4fsharp/Dojo-Canopy-2048
or via Twitter on @c4fsharp.

And if you enjoy it, feel free to tweet about it,
using the hashtag #fsharp! 
*)


(*
Task 0: play a game of 2048.
If you have never seen it before, run it here:
http://gabrielecirulli.github.io/2048/

Also, the documentation for Canopy is here:
http://lefthandedgoat.github.io/canopy/actions.html
*)

namespace Canopy2048

open canopy
open runner
open System
open Game
open Interactions

module program = 

    canopy.configuration.optimizeBySkippingIFrameCheck <- true
    canopy.configuration.optimizeByDisablingCoverageReport <- true

    "starting a game of 2048" &&& fun _ ->

        printfn "Game started."
            
        (*
        Task 1: start your browser.
        depending on what browser you prefer, 
        you may have to install a browser driver
        on your machine.
        *)

        start firefox

        (*
        Task 2: open the game url.
        *)

        url "http://gabrielecirulli.github.io/2048/"

        (*
        Task 3: play up, left, down, right.
        *)
        let task3 () =
            press up
            press left
            press down
            press right

        (*
        Task 4: retrieve the score.
        Check the page and look for the 
        appropriate css tag.
        *)

        let task4 () = read ".score-container" |> Console.WriteLine

        (*
        Task 5: play a random game.
        Write a bot that will randomly play
        up, left, down or right at each turn.
        A loop based on recursion is probably 
        a good way to go!
        *)

        let randomMove () =
            (new System.Random()).Next 4
            |> function | 0 -> Up
                        | 1 -> Left
                        | 2 -> Down
                        | _ -> Right

        let doMove =
            function | Up -> press up
                     | Left -> press left
                     | Down -> press down
                     | Right -> press right

        let task6 () =
            while true do
                randomMove () |> doMove

        (*
        Task 6: terminate!
        Improve your bot - it would be nice if
        the bot stopped playing when the game
        is either lost or won!
        The module Interactions contains 2 handy
        functions that will identify these 2
        situations, lost () and won ().
        *)

        let task6 () =
            let mutable play = true
            while play do
                randomMove () |> doMove
                if (lost () || won ()) then play <- false

        (*
        Task 7: gimme some brains!
        Let's give the bot a minimum of brains.
        Instead of playing randomly, at each
        turn, let's analyze each of the 4 possible
        moves, and pick the move that gives you
        the best one-shot score.
        The module Interactions contains a function
        that will extract the State of the game 
        from the page; it is represented as a Map,
        containing non-empty cells. 
        The keys are of type Pos, of the form 
        { Row = 1; Column = 2 }, the values are the
        integer number in the cell.
        The module Game contains a function execute,
        which takes in a State and a Move, and
        returns a tuple:
        * the score increase of the move,
        * the State after the move.
        *)

        let moves = [Up; Left; Right; Down]
        
        let bestMove () =
            let st = state ()
            moves
            (* calculate the resulting scores and states of each move *)
            |> List.map (fun move -> (move,execute st move))
            (* filter out those moves that get stuck *)
            |> List.filter (fun (move,(score,st')) -> st <> st')
            (* maximize by the score *)
            |> List.maxBy (fun (move,(score,st')) -> score)
            (* get the resulting move *)
            |> fun (move,(score,st')) -> move

        let task7 () =
            let mutable play = true
            while play do
                bestMove () |> doMove
                if (lost () || won ()) then play <- false

        (*
        Task 8: go for it!
        Now that you have a dumb bot, your turn
        to experiment and see if you can make that bot
        smarter!
        *)
      
        (* Construct depth 2 & 3 forests of moves. *)
        let moves2 = List.zip moves (List.replicate 4 moves)
        let moves3 = List.zip moves (List.replicate 4 moves2)

        (*
        Using move2, look ahead a move to figure out which move
        for this round will give a score maximizing opportunity up
        to one round ahead.
        *)
        let bestMove2 () =
            let st = state ()
            moves2
            |> List.map (fun (move,nexts) -> ((move,nexts),execute st move))
            |> List.filter (fun ((move,nexts),(score,st')) -> st <> st')
            |> List.maxBy (fun ((move,nexts),(score,st')) ->
                nexts
                |> List.map (fun move -> (move,execute st' move))
                |> List.filter (fun (move,(score,st'')) -> st' <> st'')
                |> List.maxBy (fun (move,(score',st'')) -> max score score')
                |> fun (move,(score',st'')) -> max score score')
            |> fun ((move,nexts),(score,st')) -> move

        (*
        Using move3, look ahead two move to figure out which move
        for this round will give a score maximizing opportunity up
        to two round ahead.
        *)
        let bestMove3 () =
            let st = state ()
            moves3
            |> List.map (fun (move,move2s) -> ((move,move2s),execute st move))
            |> List.filter (fun ((move,move2s),(score,st')) -> st <> st')
            |> List.maxBy (fun ((move,move2s),(score,st')) ->
                move2s
                |> List.map (fun (move,move1s) -> ((move,move1s),execute st' move))
                |> List.filter (fun ((move,move1s),(score',st'')) -> st' <> st'')
                |> List.maxBy (fun ((move,move1s),(score',st'')) ->
                    move1s
                    |> List.map (fun move -> (move,execute st'' move))
                    |> List.filter (fun (move,(score'',st''')) -> st'' <> st''')
                    |> List.maxBy (fun (move,(score'',st''')) -> score + score' + score'')
                    |> fun (move,(score'',st''')) -> score + score' + score''))
            |> fun ((move,move2s),(score,st')) -> move

        (*
        This strategy will generalize to allow lookahead of any
        number of rounds. Construct a forest of moves of depth n. Then
        calculate the best move recursively. However, the further out
        the worse the calculation because it naively ignores new blocks.
        In fact bestMove3 does worse than bestMove2. Instead, we should
        calculate expected scores over the possible distribution of a new
        block on each round. Also, absolute scores don't matter; we should
        maximize on each round by the weighted deviation from the average
        expected score (x-m)/m. Then maximize over the rounds, weighing
        sooner rounds higher than later rounds, to make the bot risk averse.
        *)

        let task8 () =
            let mutable play = true
            while play do
                bestMove2 () |> doMove
                if (lost () || won ()) then play <- false

        task8 ()

        // Just to make sure the test function
        // ends properly.
        printfn "Game ended."
        ignore ()


    //run the "test", i.e. play a game
    run()

    printfn "Press [enter] to exit."
    System.Console.ReadLine() |> ignore

    quit()