//
// This file is part of an OMNeT++/OMNEST simulation example.
//
// Copyright (C) 2003 Ahmet Sekercioglu
// Copyright (C) 2003-2008 Andras Varga
//
// This file is distributed WITHOUT ANY WARRANTY. See the file
// `license' for details on this and other legal matters.
//

#include <stdio.h>
#include <string.h>
#include <omnetpp.h>
#include "tictoc16_m.h"

using namespace omnetpp;

/**
 * The main problem with the previous step is that we must modify the model's
 * code if we want to change what statistics are gathered. Statistic calculation
 * is woven deeply into the model code which is hard to modify and understand.
 *
 * OMNeT++ 4.1 provides a different mechanism called 'signals' that we can use
 * to gather statistics. First we have to identify the events where the state
 * of the model changes. We can emit signals at these points that carry the value
 * of chosen state variables. This way the C++ code only emits signals, but how those
 * signals are processed are determined only by the listeners that are attached to them.
 *
 * The signals the model emits and the listeners that process them can be defined in
 * the NED file using the 'signal' and 'statistic' property.
 *
 * We will gather the same statistics as in the previous step, but notice that we will not need
 * any private member variables to calculate these values. We will use only a single signal that
 * is emitted when a message arrives and carries the hopcount in the message.
 */
class Txc : public cSimpleModule
{
  private:
	simsignal_t arrivalSignal;
	bool randomGate;

  protected:
    virtual TicTocMsg16 *generateMessage();
    virtual void forwardMessage(TicTocMsg16 *msg);
    virtual void initialize() override;
    virtual void handleMessage(cMessage *msg) override;
};

Define_Module(Txc);

void Txc::initialize()
{
    randomGate = par("randomGate").boolValue();

    arrivalSignal = registerSignal("arrival");
    // Module 0 sends the first message
    if (getIndex()==0)
    {
        // Boot the process scheduling the initial message as a self-message.
        TicTocMsg16 *msg = generateMessage();
        scheduleAt(0.0, msg);
    }
}

void Txc::handleMessage(cMessage *msg)
{
    TicTocMsg16 *ttmsg = check_and_cast<TicTocMsg16 *>(msg);

    if (ttmsg->getDestination()==getIndex())
    {
        // Message arrived
        int hopcount = ttmsg->getHopCount();
        // send a signal
        emit(arrivalSignal, hopcount);

        EV << "Message " << ttmsg << " arrived after " << hopcount << " hops.\n";
        bubble("ARRIVED, simulation finished!");

        delete ttmsg;
        // Following lines are commented. So no other message will be sent.
//        // Generate another one.
//        EV << "Generating another message: ";
//        TicTocMsg16 *newmsg = generateMessage();
//        EV << newmsg << endl;
//        forwardMessage(newmsg);
    }
    else
    {
        // We need to forward the message.
        forwardMessage(ttmsg);
    }
}

TicTocMsg16 *Txc::generateMessage()
{
    // Produce source and destination addresses.
    int src = getIndex();
    int n = getVectorSize();
//    int dest = intuniform(0,n-2);
//    if (dest>=src) dest++;
//  Instead of random destination (which differ from repetition-to-repetition, for seed=${repetition})
//  here we use the last node as the destination.
    int dest = n-1;

    char msgname[20];
    sprintf(msgname, "tic-%d-to-%d", src, dest);

    // Create message object and set source and destination field.
    TicTocMsg16 *msg = new TicTocMsg16(msgname);
    msg->setSource(src);
    msg->setDestination(dest);
    return msg;
}

void Txc::forwardMessage(TicTocMsg16 *msg)
{
    // Increment hop count.
    msg->setHopCount(msg->getHopCount()+1);

    // If using randomGate is TRUE, the implementation is the same as in Txc16.
    // If randomGate is FALSE then random gate selection will not choose the incoming gate.
    int n = gateSize("gate");
    int k;

    // The incoming gate can be used if:
    // 1. The randomGate is TRUE; or
    // 2. The module only has 1 gate.
    // getIndex()==0 used by the initial sender.
    if(randomGate||getIndex()==0||n==1)
    {
        k = intuniform(0,n-1);
    } else {
        do {
            k = intuniform(0,n-1);
        } while (k==msg->getArrivalGate()->getIndex());
    }

    EV << "Forwarding message " << msg << " on gate[" << k << "]\n";
    send(msg, "gate$o", k);
}
