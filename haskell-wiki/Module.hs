module ModuleA where

import Data.Char (toLower, toUpper) --import two functions from the module 
import Data.List -- Import everything from the module Data.List

import Data.Tree (Tree(Node)) --Import only the Tree data type and its node constructor 

-- qualified imports: usefull when there cna be ambigous function definitions in modules/packages. Example of importing functions with common names from different modules
import qualified MyModule
import qualified AnotherModule

let result1 = MyModule.fooBar
let result2 = AnotherModule.fooBar

--Hiding definitions
-- From the above example, if we need only the definition of 'fooBar' present in AnotherModule, we can hide the definition in MyModule from being imported. NB: algebraic datatypes and type synonyms cannot be hidden.

import MyModule hiding (fooBar) -- fooBar in this module will not be imported


--Renaming imports: Can be used for instance to shorten a qualified module name
import qualified SomeModuleWithAVeryLongName as Shortened

--Shortened.fooBar

--Exporting: Deciding which functions are external and which ones stay internal

