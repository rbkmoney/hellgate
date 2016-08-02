#!groovy

node('docker-host') {
  stage 'git checkout'
  checkout scm

  loadBuildUtils()

  stage 'load pipeline'
  env.JENKINS_LIB = "build_utils/jenkins_lib"
  def pipeline = load("${env.JENKINS_LIB}/pipeline.groovy")

  pipeline("hellgate", '_build/') {

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
        sh "make build_image"
      }

      runStage('push image') {
        sh "make push_image"
      }
    }
  }
}

