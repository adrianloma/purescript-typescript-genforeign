'use strict';

const _ts = require ('typescript');

const syntaxKind = {};

(() =>
 {for (const name of Object.keys(_ts.SyntaxKind).filter(x => isNaN(parseInt(x)))) {
    const value = _ts.SyntaxKind[name];
    if (!syntaxKind[value]) {
        syntaxKind[value] = name;
    }
 }
 })();

exports.getSyntaxKindForInt = function(intSyntaxKind){
    if (syntaxKind[intSyntaxKind]) {
        return syntaxKind[intSyntaxKind];
    }
    return null;
}
