// Store Erlang Common Test Log by it's rebar location
def call(String artiFacts) {
  if (!fileExists('_build/test/logs/index.html')) {
    runStage('store CT log') {
      step([$class: 'ArtifactArchiver', artifacts: '_build/test/logs/', fingerprint: true])
    }
  }
}

return this;

