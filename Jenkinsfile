#!groovy

// Args:
// GitHub repo name
// Jenkins agent label
// Tracing artifacts to be stored alongside build logs
// Optional: artifacts to cache between the builds
pipeline("cds", 'docker-host', "_build/") {

    // ToDo: Uncomment the stage as soon as Elvis is in the build image!
    // runStage('lint') {
    //   sh 'make w_container_lint'
    // }

    runStage('compile') {
     sh 'make w_container_compile'
    }

    runStage('xref') {
     sh 'make w_container_xref'
    }

    runStage('test') {
     sh "make w_container_test"
    }

    runStage('dialyze') {
     sh 'make w_container_dialyze'
    }
}
