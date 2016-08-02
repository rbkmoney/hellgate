def call(String name, Closure body) {
  env.STAGE_NAME = name
  stage "${env.STAGE_NAME}"
  body.call()
}

return this;

