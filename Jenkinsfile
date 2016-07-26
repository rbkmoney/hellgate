#!groovy

// Args:
// GitHub repo name
// Jenkins agent label
// Tracing artifacts to be stored alongside build logs
pipeline("hellgate", 'docker-host', "_build/") {

  runStage('submodules') {
    sh 'make wc_submodules'
  }

  runStage('compile') {
    sh 'make wc_compile'
  }

  runStage('lint') {
    sh 'make wc_lint'
  }

  runStage('xref') {
    sh 'make wc_xref'
  }

  runStage('dialyze') {
    sh 'make wc_dialyze'
  }

  runStage('test') {
    sh "make wdeps_test"
  }

  if (env.BRANCH_NAME == 'master') {
    runStage('build image') {
      sh "IMAGE_TAG=${env.BUILD_ID} make containerize"
    }

    runStage('push image :commit') {
      sh "IMAGE_TAG=${env.BUILD_ID} PUSH_IMAGE_TAG=`git rev-parse --short HEAD`_`date +"%s"` make push"
    }

    runStage('push image :latest') {
      sh "IMAGE_TAG=latest make push"
    }
  }
}

