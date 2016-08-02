def call(String repoName, String artiFacts = null, Closure body) {
  try {
    docker.withRegistry('https://dr.rbkmoney.com/v2/', 'dockerhub-rbkmoneycibot') {

      stage 'init pipeline'
      def runStage = load("${env.JENKINS_LIB}/runStage.groovy")
      def storeArtifacts = load("${env.JENKINS_LIB}/storeArtifacts.groovy")
      env.REPO_NAME = repoName
      def buildImg = docker.image('rbkmoney/build:latest')
      //sh 'git submodule update --init'
      sh 'git --no-pager log -1 --pretty=format:"%an" > .commit_author'
      env.COMMIT_AUTHOR = readFile('.commit_author').trim()

      runStage('pull build image') {
        buildImg.pull()
      }

      wrap([$class: 'AnsiColorBuildWrapper', 'colorMapName': 'XTerm']) {
        body.call()
      }

      stage 'notify slack'
      slackSend color: 'good', message: "<${env.BUILD_URL}|Build ${env.BUILD_NUMBER}> for ${env.REPO_NAME} by ${env.COMMIT_AUTHOR} has passed on branch ${env.BRANCH_NAME} (jenkins node: ${env.NODE_NAME})."

    }
  } catch (Exception e) {
    stage 'notify slack'
    slackSend color: 'danger', message: "<${env.BUILD_URL}|Build ${env.BUILD_NUMBER}> for ${env.REPO_NAME} by ${env.COMMIT_AUTHOR} has failed on branch ${env.BRANCH_NAME} at stage: ${env.STAGE_NAME} (jenkins node: ${env.NODE_NAME})."

    throw e; // rethrow so the build is considered failed
  } finally {
    if (artiFacts != null) {
      runStage('store artifacts') {
        storeArtifacts(artiFacts)
      }
    }

    runStage('wipe workspace') {
      sh 'docker run --rm -v $PWD:$PWD --workdir $PWD rbkmoney/build:latest /bin/bash -c "rm -rf * .* 2>/dev/null || echo ignore"'
    }
  }
}

// Note: 'scm checkout' invokes 'git checkout -f', which purges everything from the workspace
// making the following functions useless for now :-(
def readCache(String cacheItems) {
  runStage('read cache') {
    try {
      sh "ls -al .jenkins.cache/${cacheItems}"
      sh "mkdir -p `dirname ${cacheItems}` && cp -r .jenkins.cache/${cacheItems} `dirname ${cacheItems}`/"
    } catch (Exception noCache) {}
  }
}

def writeCache(String cacheItems) {
  runStage('write cache') {
    sh "ls -al ${cacheItems}"
    sh "mkdir -p .jenkins.cache/`dirname ${cacheItems}` && cp -r ${cacheItems} .jenkins.cache/`dirname ${cacheItems}`/"
  }
}

return this;

