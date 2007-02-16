#!/bin/sh

mkdir -p build

if [ -f rhino1_6R5.zip ] 
then
    echo "using local rhino\n"
else
    wget ftp://ftp.mozilla.org/pub/mozilla.org/js/rhino1_6R5.zip
fi

if [ -f narrativejs-beta1.zip ] 
then
    echo "using local narrativejs\n"
else
    wget http://www.neilmix.com/narrativejs/narrativejs-beta1.zip
fi

unzip -j ./narrativejs-beta1.zip  narrativejs/src/Narcissus.js 
unzip -j ./narrativejs-beta1.zip  narrativejs/src/NjsCompiler.js 
unzip -j ./narrativejs-beta1.zip  narrativejs/src/NjsScriptWriter.js
unzip -j ./narrativejs-beta1.zip  narrativejs/src/compiler_main.js

cat Narcissus.js NjsCompiler.js NjsScriptWriter.js compiler_main.js > Compiler.js


# extract org/mozilla/javascript/resources/Messages.properties from rhino.zip
unzip -j ./rhino1_6R5.zip rhino1_6R5/src/org/mozilla/javascript/resources/Messages.properties

# rename Messages.properties to RhinoMessages.properties
mv Messages.properties build/RhinoMessages.properties

# build RhinoMessages.o
gcj -c --resource=org/mozilla/javascript/resources/Messages.properties -o build/RhinoMessages.o RhinoMessages.properties

# extract org/mozilla/javascript/tools/resources/Messages.properties from rhino.zip
unzip -j rhino1_6R5.zip rhino1_6R5/toolsrc/org/mozilla/javascript/tools/resources/Messages.properties

# rename Messages.properties to RhinoToolsMessages.properties
mv Messages.properties build/RhinoToolsMessages.properties

# build RhinoToolsMessages.o
gcj -c --resource=org/mozilla/javascript/tools/resources/Messages.properties -o build/RhinoToolsMessages.o RhinoToolsMessages.properties

# extract rhino source from rhino.zip to temp
unzip rhino1_6R5.zip rhino1_6R5/js.jar  -d temp
unzip rhino1_6R5.zip rhino1_6R5/src/org/* -d temp
unzip rhino1_6R5.zip rhino1_6R5/toolsrc/org/* -d temp


java -cp temp/rhino1_6R5/js.jar  org.mozilla.javascript.tools.jsc.Main Compiler.js

mv Compiler.class temp/rhino1_6R5/src

rm *.js

# build mod_js.o
find temp/rhino1_6R5/src temp/rhino1_6R5/toolsrc -type f \( -name "*.java" -o -name "*.class" \) -not -path "*/tools/debugger*" -not -name ConsoleTextArea.java -not -name JSConsole.java | xargs gcj -c -Os -o build/mod_js.o

gcj --main=Compiler -o build/narrativejs build/mod_js.o

mkdir -p ../lib/tercio/priv
mv build/narrativejs ../lib/tercio/priv

# clean up
rm -rf build
rm -rf temp
