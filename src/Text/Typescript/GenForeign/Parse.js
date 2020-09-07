'use strict';

var ts = require("typescript");

/** Generate documentation for all classes in a set of .ts files */
exports.generateDocumentationForInputString = function(inputString){
    let options = {
          target: ts.ScriptTarget.ES5
        , module: ts.ModuleKind.CommonJS
    };

    return generateDocumentationForInputStringAndOptions(inputString, options);
};

function generateDocumentationForInputStringAndOptions(inputString, options){
    let sourceMapText = "";
    let outputText = "";

    options.suppressOutputPathCheck = true;
    options.allowNonTsExtensions = true;
    const target = options.target | ts.ScriptTarget.ES5;
    const inputFileName = "module.ts";
    const sourceFile = ts.createSourceFile(inputFileName, inputString, target);

    var compilerHost =
        { getSourceFile: ((fileName) =>  fileName === ts.normalizePath(inputFileName) ? sourceFile : undefined)
          , writeFile: (
              (name, text) =>
                  (fileExtensionIs(name, ".map")) ?
                  sourceMapText = text :
                  outputText = text
            )
        , getDefaultLibFileName: (() => "lib.d.ts")
        , useCaseSensitiveFileNames: (() =>  false)
        , getCanonicalFileName: ((fileName) =>  fileName)
        , getCurrentDirectory: (() =>  "")
        , getNewLine: (() =>  newLine)
        , fileExists: ((fileName) =>  fileName === inputFileName)
        , readFile: (() =>  "")
        , directoryExists: (() =>  true)
        , getDirectories: (() =>  [])
        };

    let program = ts.createProgram([inputFileName], options, compilerHost);

    return generateDocumentationForProgram(program);
}

function generateDocumentationForFilesAndOptions(fileNames, options){
    // Build a program using the set of root file names in fileNames
    let program = ts.createProgram(fileNames, options);
    return generateDocumentationForProgram(program);
}

/** Generate documentation for all classes in a set of .ts files */
function generateDocumentationForProgram(program) {
    // Get the checker, we will use it to find more about classes
    let checker = program.getTypeChecker();
    let output = { functions : []
                   , classes : []
                   , enums : []
                   , interfaces : []
                 };
    // Visit every sourceFile in the program
    for (const sourceFile of program.getSourceFiles()) {
        if (!sourceFile.isDeclarationFile) {
            // Walk the tree to search for classes
            ts.forEachChild(sourceFile, visit);
        }
    }

    // print out the doc
    return JSON.stringify(output, undefined, 4);

    function getSignatureForDeclarationKind(parentNode, signatureKind){
        var declarations = [];

        ts.forEachChild(parentNode, visitChildNodeOfParent);

        return declarations;


        function visitChildNodeOfParent(node){
            if (node.kind === signatureKind
                && signatureKind === ts.SyntaxKind.MethodDeclaration
                && node.name) {
                let symbol = checker.getSymbolAtLocation(node.name);
                if(symbol.getName() === "toJSON") return;
                if (symbol) {
                    let declarationDetails = serializeParameterNode(node);
                    declarationDetails.params = node.parameters.map(serializeParameterNode);
                    declarations.push(declarationDetails);
                }
            }
            if (node.kind === signatureKind && signatureKind === ts.SyntaxKind.Constructor){
                let classNode = node.parent;
                let classSymbol = checker.getSymbolAtLocation(classNode.name);
                let typeConstructor = checker.getTypeOfSymbolAtLocation(
                                        classSymbol,
                                        classSymbol.valueDeclaration);
                declarations = typeConstructor
                    .getConstructSignatures()
                    .map((constructor) => (
                        { name: "constructor" + classSymbol.getName()
                          , params: node.parameters.map(serializeParameterNode)
                          , tsType: classSymbol.getName()
                          , docs: ts.displayPartsToString(constructor.getDocumentationComment(checker))
                        }
                ));
            }

            if (node.kind === signatureKind && signatureKind === ts.SyntaxKind.EnumDeclaration){
                declarations.push(serializedEnum);
            }
            if (node.kind === signatureKind && signatureKind === ts.SyntaxKind.EnumMember){
                let symbol = checker.getSymbolAtLocation(node.name);
                if(!symbol) return;
                declarations.push(symbol.getName());
            }
            if (node.kind === signatureKind && signatureKind === ts.SyntaxKind.PropertySignature){
                let declarationDetails = serializeParameterNode(node);
                declarations.push(declarationDetails);
            }
        };
    }

    /** visit nodes finding exported classes */
    function visit(node) {
        // Only consider exported nodes
        if (!isNodeExported(node)) {
            return;
        }

        if (ts.isModuleDeclaration(node)) {
            // This is a namespace, visit its children
            ts.forEachChild(node, visit);
            return;
        }

        let symbol = checker.getSymbolAtLocation(node.name);
        if(!symbol) return;

        if (ts.isClassDeclaration(node) && node.name) {
            let serializedClass = {
                name: symbol.getName()
                , docs: ts.displayPartsToString(symbol.getDocumentationComment(checker))
                , constructors: getSignatureForDeclarationKind(node, ts.SyntaxKind.Constructor)
                , methods: getSignatureForDeclarationKind(node, ts.SyntaxKind.MethodDeclaration)
            };
            output.classes.push(serializedClass);
        } else if (ts.isFunctionDeclaration(node)){
           let serializedFunction = {
                name: symbol.getName()
                , docs: ts.displayPartsToString(symbol.getDocumentationComment(checker))
                , params: node.parameters.map(serializeParameterNode)
            };
            serializedFunction = serializeParameterNode(node);
            serializedFunction.params = node.parameters.map(serializeParameterNode);

            output.functions.push(serializedFunction);
        } else if (ts.isEnumDeclaration(node)){
            let serializedEnum = {
                name: symbol.getName()
                , members: getSignatureForDeclarationKind(node, ts.SyntaxKind.EnumMember)
            };
            output.enums.push(serializedEnum);
        } else if (ts.isInterfaceDeclaration(node)){
            let serializedInterface = {
                name: symbol.getName()
                , properties: getSignatureForDeclarationKind(node, ts.SyntaxKind.PropertySignature)
            };
            output.interfaces.push(serializedInterface);
        }
    }

    /** Serialize a symbol into a json object */
    function serializeParameterNode(node) {
        let symbol = checker.getSymbolAtLocation(node.name);
        let serializedParam = {
            name: symbol.getName()
            , docs: ts.displayPartsToString(symbol.getDocumentationComment(checker))
            , tsType: serializeNestedType(node.type)
        };
        if(node.questionToken){
            serializedParam.tsType = { typeConstructor: "Maybe"
                                               , typeParams: serializedParam.tsType
                                             };
        }
        return serializedParam;

        function serializeNestedType(nextType){
            if(!nextType) return "void";
            switch(nextType.kind){
            case ts.SyntaxKind.ArrayType:
                return { typeConstructor: "Array"
                         , typeParams: serializeNestedType(nextType.elementType)
                       };
            case ts.SyntaxKind.TypeReference:
                if(nextType.typeArguments === undefined){
                    if (nextType.getText() == "Function"){
                        return "JsFunction"; // reserved word
                    }
                    return nextType.getText();
                } else {
                    return { typeConstructor: nextType.typeName.getText()
                             , typeParams: nextType.typeArguments.map(serializeNestedType)
                           };
                }
            case ts.SyntaxKind.UnionType:
                // name will be used to create union type signature
                return { name: symbol.getName()
                       , unionTypes: nextType.types.map(serializeNestedType)
                       };
            case ts.SyntaxKind.TypeLiteral:
                return { name: ""
                         , properties: getSignatureForDeclarationKind(
                                            nextType
                                            , ts.SyntaxKind.PropertySignature)
                       };
            case ts.SyntaxKind.ThisType:
                let thisSymbol = checker.getSymbolAtLocation(nextType);
                return thisSymbol.getName();
            case ts.SyntaxKind.FunctionType:
                return {
                    name: symbol.getName() + "callBack"
                    , docs: ""
                    , tsType: serializeNestedType(nextType.type)
                    , params: nextType.parameters.map(serializeNestedType)
                };
            default:
                return nextType.getText();
            }
        }

    }

    /** True if this is visible outside this file, false otherwise */
    function isNodeExported(node) {
        return ((ts.getCombinedModifierFlags(node) & ts.ModifierFlags.Export) !== 0 ||
            (!!node.parent && node.parent.kind === ts.SyntaxKind.SourceFile));
    }
}

