#!/bin/bash

ghc -i. Biblioteca/Main.hs -o program

rm Biblioteca/*.o

rm Biblioteca/*.hi