pipeline {
    agent { dockerfile true }
    stages {
        stage('build') {
            steps {
                sh 'stack build'
            }
        }
        stage('test'){
           steps {
           sh 'stack test'
           }
        }
    }
}
