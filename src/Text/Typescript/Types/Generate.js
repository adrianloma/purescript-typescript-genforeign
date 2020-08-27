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
    var compilerHost = {
    getSourceFile: function getSourceFile(fileName) {
        return fileName === ts.normalizePath(inputFileName) ? sourceFile : undefined;
    },
    writeFile: function writeFile(name, text) {
        if (fileExtensionIs(name, ".map")) {
        sourceMapText = text;
        } else {
        outputText = text;
        }
    },
    getDefaultLibFileName: function getDefaultLibFileName() {
        return "lib.d.ts";
    },
    useCaseSensitiveFileNames: function useCaseSensitiveFileNames() {
        return false;
    },
    getCanonicalFileName: function getCanonicalFileName(fileName) {
        return fileName;
    },
    getCurrentDirectory: function getCurrentDirectory() {
        return "";
    },
    getNewLine: function getNewLine() {
        return newLine;
    },
    fileExists: function fileExists(fileName) {
        return fileName === inputFileName;
    },
    readFile: function readFile() {
        return "";
    },
    directoryExists: function directoryExists() {
        return true;
    },
    getDirectories: function getDirectories() {
        return [];
    }
    };

    let program = ts.createProgram([inputFileName], options, compilerHost);

    return generateDocumentationForProgram(program);
}

function generateDocumentationForFilesAndOptions(fileNames, options){
    generateDocumentationForFilesAndOptions(fileNames, options);
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
    let output = [];
    // Visit every sourceFile in the program
    for (const sourceFile of program.getSourceFiles()) {
        if (!sourceFile.isDeclarationFile) {
            // Walk the tree to search for classes
            ts.forEachChild(sourceFile, visit);
        }
    }

    // print out the doc
    return JSON.stringify(output, undefined, 4);

    /** visit nodes finding exported classes */
    function visit(node) {
        // Only consider exported nodes
        if (!isNodeExported(node)) {
            return;
        }
        if (ts.isClassDeclaration(node) && node.name) {
            // This is a top level class, get its symbol
            let symbol = checker.getSymbolAtLocation(node.name);
            if (symbol) {
                output.push(serializeClass(symbol));
            }
            // No need to walk any further, class expressions/inner declarations
            // cannot be exported
        }
        else if (ts.isModuleDeclaration(node)) {
            // This is a namespace, visit its children
            ts.forEachChild(node, visit);
        }
    }
    /** Serialize a symbol into a json object */
    function serializeSymbol(symbol) {
        return {
            name: symbol.getName(),
            documentation: ts.displayPartsToString(symbol.getDocumentationComment(checker)),
            type: checker.typeToString(checker.getTypeOfSymbolAtLocation(symbol, symbol.valueDeclaration))
        };
    }
    /** Serialize a class symbol information */
    function serializeClass(symbol) {
        let details = serializeSymbol(symbol);
        // Get the construct signatures
        let constructorType = checker.getTypeOfSymbolAtLocation(symbol, symbol.valueDeclaration);
        details.constructors = constructorType
            .getConstructSignatures()
            .map(serializeSignature);
        return details;
    }
    /** Serialize a signature (call or construct) */
    function serializeSignature(signature) {
        return {
            parameters: signature.parameters.map(serializeSymbol),
            returnType: checker.typeToString(signature.getReturnType()),
            documentation: ts.displayPartsToString(signature.getDocumentationComment(checker))
        };
    }
    /** True if this is visible outside this file, false otherwise */
    function isNodeExported(node) {
        return ((ts.getCombinedModifierFlags(node) & ts.ModifierFlags.Export) !== 0 ||
            (!!node.parent && node.parent.kind === ts.SyntaxKind.SourceFile));
    }
}

