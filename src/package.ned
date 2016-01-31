//
//@titlepage
//@author Daniel Febrian Sengkey
//@version 1.0
//<h1> Sample Project of Processing Results from OMNeT++ with R </h1>
//<h2> Introduction </h2>
//This is a simple project as an example of simulation result processing.
//The example is derived from TicToc example in the OMNeT++ sample projects by
//Andras Varga and Rudolf Hornig.
//
//<h2> Disclaimer </h2>
//I don't intent to violate any copyright here. My goal is just to provide
//an example of post-simulation analysis, using R. So if you think something
//in this project violate someone intellectual property (e.g. not properly
//write the copyright statement, ownership, authors of the original source)
//please drop me an e-mail. The address is down below.
//
//<h2> Modification </h2>
//This example use a modified node based on the TicToc project. You may
//notice that since TicToc5 more nodes are added. It uses random gate selection (with
//uniform probability) to forward received message. Node in this project uses similar
//method of forwarding. However, I also add a restriction using simple do..while to
//prevent the message sent back through its incoming gate (This is also a <i>quest</i>
//the tutorial though). An exception happens when the node only has singe IO gate.
//The restriction can be set from module parameter. Therefore we
//can set it for different configurations in omnetpp.ini file.
//
//<h2> Hop Count </h2>
//The hop count is a network metric that measured here. It is clear that pure random
//routing is very prone to routing loop, and therefore more hops. Avoid sending through
//the incoming gate is cleary reduce the hop count.
//
//<h2> R Source </h2>
//Example of the R source file (and some plots) available in the R directory. Since
//this project is to serve as a basis of learning OMNeT++ result analysis in R, there is
//no advanced graph libary (such as ggplot or lattice) used.
//
//<h2> Contact </h2>
//I am well aware of my capability of coding and writing documentation. Therefore I
//really need your advises and even critics. You can contact me via my e-mail:
//<b>danielfebrian015@gmail.com</b>.
//

package resultprocessing;

@license(LGPL);
