var app = new Vue({
    "el": "#tabs",
    "data": {
        "tab": "validate",
        "validator": {
            "inputFormat": "lmf",
            "inputRdfLang": "RDF/XML",
            "inputText": ""
        },
        "converter": {
            "inputFormat": "lmf",
            "inputRdfLang": "RDF/XML",
            "outputFormat": "lmf",
            "outputRdfLang": "RDF/XML",
            "inputText": ""
        }
    }
});
