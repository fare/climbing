#lang at-exp racket

(require scribble/base scribble/manual scriblib/autobib "utils.rkt")

(provide (all-defined-out))

(define-bib FarePhD
  #:author "François-René Rideau"
  #:title "Reconciling Semantics and Reflection"
  ;; #:url "http://j.mp/FarePhD"
  #:date "2018")

(define-bib PCLSRing
  #:title "PCLSRing: Keeping Process State Modular"
  #:author "Alan Bawden"
  ;; #:institution "MIT"
  ;; #:url "http://fare.tunes.org/tmp/emergent/pclsr.htm"
  #:date "1989")

(define-bib MilnerSimulation1971
  #:title "An Algebraic Definition of Simulation between Programs"
  #:author "Robin Milner"
  #:date "1971")

(define-bib Howard1980
  #:title "The formulae-as-types notion of construction"
  #:author "William A. Howard"
  #:date "1980") ;; [original paper manuscript from 1969]

(define-bib Norell08Agda
  #:title "Dependently typed programming in Agda"
  #:author "Ulf Norell"
  #:date "2008")

(define-bib Smith1982Reflection
  #:title "Procedural Reflection in Programming Languages"
  #:author "Brian Cantwell Smith"
  #:date "1982")

(define-bib Erlang1993
  #:title "Concurrent Programming in ERLANG"
  #:author "Joe Armstrong and Robert Virding and Claes Wikström and Mike Williams"
  #:date "1993")

(define-bib UPGCTR1992
  #:title "Uniprocessor Garbage Collection Techniques"
  #:author "Paul R. Wilson"
  #:date "1992")

(define-bib Feeley2015Migration
  #:title "Compiling for Multi-language Task Migration"
  #:author "Marc Feeley"
  #:date "2015")
