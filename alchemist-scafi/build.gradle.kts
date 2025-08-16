plugins {
    java
    scala
}

group = "io.github.davidedomini"

repositories {
    mavenCentral()
}

dependencies {
    implementation(libs.scala2)
    implementation(libs.alchemist)
    implementation(libs.alchemistScafi)
    implementation(libs.alchemistProtelis)
    implementation(libs.alchemistGui)
    implementation(libs.slf4j)
    implementation(libs.logback)
    testImplementation(libs.junit.api)
    testRuntimeOnly(libs.junit.engine)
    implementation(project(":scarlib-core"))
    implementation(project(":dsl-core"))
    implementation("org.apache.spark:spark-core_2.12:3.5.0")
    implementation("org.apache.spark:spark-sql_2.12:3.5.0")
    implementation("org.apache.spark:spark-mllib_2.12:3.5.0")
    implementation ("org.apache.spark:spark-streaming_2.12:3.5.0")
    // Spark Streaming Kafka integration (if using Kafka)
    implementation ("org.apache.spark:spark-streaming-kafka-0-10_2.12:3.5.0")
}

tasks.getByName<Test>("test") {
    useJUnitPlatform()
}

publishing.publications {
    withType<MavenPublication> {
        pom {
            developers {
                developer {
                    name.set("Gianluca Aguzzi")
                    email.set("gianluca.aguzzi@unibo.it")
                }
            }
        }
    }
}
