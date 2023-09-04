module.exports = {
    apps : [{
      name: "winnar",
      script: "next start",
      env: {
        NODE_ENV: "development",
      },
      env_production: {
        NODE_ENV: "production",
      }
    }]
  }