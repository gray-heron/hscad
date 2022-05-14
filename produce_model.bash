#!/bin/bash

stack exec -- runghc Main.hs "`cat settings | sed -n 1p`" "`cat settings | sed -n 2p`" > model.scad