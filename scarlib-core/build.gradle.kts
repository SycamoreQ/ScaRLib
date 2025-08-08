plugins {
    java
    scala
}

group = "io.github.davidedomini"

scala {
    zincVersion.set("1.6.1")
}

sourceSets {
    main {
        scala {
            setSrcDirs(listOf("src/main/scala"))
        }
    }
    test {
        scala {
            setSrcDirs(listOf("src/test/scala"))
        }
    }
}

repositories {
    mavenCentral()
}

dependencies {
    implementation(libs.scala2)
    implementation(libs.scalapy)
    testImplementation(libs.scalaTest)
    testImplementation(libs.scalaTestPlus)
    testImplementation(libs.junit)
    implementation("org.apache.spark:spark-core_2.12:3.5.0")
    implementation("org.apache.spark:spark-sql_2.12:3.5.0")
    implementation("org.apache.spark:spark-mllib_2.12:3.5.0")
    implementation ("org.apache.spark:spark-streaming_2.12:3.5.0")
    // Spark Streaming Kafka integration (if using Kafka)
    implementation ("org.apache.spark:spark-streaming-kafka-0-10_2.12:3.5.0")
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
