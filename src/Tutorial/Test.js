"use strict";

import {minimalSetup, EditorView} from "codemirror"
//import {parser} from './src/Tutorial/parser.js'
import {styleTags, tags as t} from "@lezer/highlight"
import {LRLanguage} from "@codemirror/language"
import {completeFromList} from "@codemirror/autocomplete"
import {foldNodeProp, foldInside, indentNodeProp} from "@codemirror/language"
import { autocompletion } from '@codemirror/autocomplete';


export const setupTextEditor = function(parser){
    global.p = parser.parse("(1 2 3 (a b c))")


    let parserWithMetadata = parser.configure({
      props: [
        styleTags({
          Identifier: t.variableName,
          Boolean: t.bool,
          String: t.string,
          LineComment: t.lineComment,
          "( )": t.paren
        }),
        indentNodeProp.add({
          Application: context => context.column(context.node.from) + context.unit
        }),
        foldNodeProp.add({
          Application: foldInside
        })
      ]
    })

    const exampleLanguage = LRLanguage.define({
      parser: parserWithMetadata,
      languageData: {
        commentTokens: {line: ";"}
      }
    })

//    const exampleCompletion = exampleLanguage.data.of({
//      autocomplete: completeFromList([
//        {label: "defun", type: "keyword"},
//        {label: "defvar", type: "keyword"},
//        {label: "let", type: "keyword"},
//        {label: "cons", type: "function"},
//        {label: "car", type: "function"},
//        {label: "cdr", type: "function"}
//      ])
//    })

    new EditorView({
      language2394823487: exampleLanguage,

      doc: "...",
      extensions: [
        minimalSetup,
        exampleLanguage.extension
        ],
      parent: document.getElementById("codemirror")
    })
}