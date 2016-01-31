//
// This file is part of an OMNeT++/OMNEST simulation example.
//
// Copyright (C) 2003-2010 Andras Varga, Rudolf Hornig
//
// This file is distributed WITHOUT ANY WARRANTY. See the file
// `license' for details on this and other legal matters.
//

package resultprocessing.simulations;
import resultprocessing.tictoc.Txc;

//
// Same as Tictoc12
//
network Tictoc16
{
    types:
        channel Channel extends ned.DelayChannel {
            delay = 100ms;
        }
    submodules:
        tic[6]: Txc;
    connections:
        tic[0].gate++ <--> Channel <--> tic[1].gate++;
        tic[1].gate++ <--> Channel <--> tic[2].gate++;
        tic[1].gate++ <--> Channel <--> tic[4].gate++;
        tic[3].gate++ <--> Channel <--> tic[4].gate++;
        tic[4].gate++ <--> Channel <--> tic[5].gate++;
}

