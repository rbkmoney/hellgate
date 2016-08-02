def call(String artiFacts) {
  try {
    def archivedArtifacts = 'artifacts.tar.gz'
    sh "tar -czf ${archivedArtifacts} ${artiFacts}"
    step([$class: 'ArtifactArchiver', artifacts: archivedArtifacts, fingerprint: true])
  } catch (Exception e) {
    echo "Error: store artifacts failed!"
    echo "Exception: ${e}"
  }
}

return this;

