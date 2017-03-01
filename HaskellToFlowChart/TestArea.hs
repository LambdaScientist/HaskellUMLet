module TestArea where


import Prelude
import FlowChart
import StringTools


import Text.PrettyPrint

import Text.PrettyPrint.HughesPJClass


foo = If [TitlePath ("FOO1", bar),TitlePath ("FOO2", bar),TitlePath ("FOO3", bar) ]

bar = Node "Code" 

-- baz = foo [bar] 