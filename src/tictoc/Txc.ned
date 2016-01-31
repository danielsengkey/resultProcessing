//
// This file is part of an OMNeT++/OMNEST simulation example.
//
// Copyright (C) 2003-2010 Andras Varga, Rudolf Hornig
//
// This file is distributed WITHOUT ANY WARRANTY. See the file
// `license' for details on this and other legal matters.
//
package resultprocessing.tictoc;

// This node is based on TicToc example
// by Andras Varga and Rudolf Hornig.
// This node has the choice to implement
// pure random routing (allow the message
// to be sent back through incoming gate),
// or restrict the use of incoming gate
// as out gate. The hopCount statistic will
// be used in result processing as an example
// of result processing using R.
// @author Andras Varga (base source)
// @author Rudolf Hornig (base source)
// @author Daniel Febrian Sengkey (modification of out gate selection)
//
simple Txc
{
    parameters:
        
        bool randomGate = default(true);
        
        @signal[arrival](type="long");
        @statistic[hopCount](title="hop count"; source="arrival"; record=last);

        @display("i=block/routing");
        
    gates:
        inout gate[];
}