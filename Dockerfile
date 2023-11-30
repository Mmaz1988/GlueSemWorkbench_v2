# Build the jar using a maven image
FROM maven:3.9-amazoncorretto-11 AS build

COPY . /home/gswb/src

RUN mvn -f /home/gswb/src/pom.xml clean package

# Start from a jdk base image
FROM eclipse-temurin:17.0.9_9-jdk

EXPOSE 8081

# install jemalloc as alternative malloc implementation (to be more robust to memory fragmentation)
RUN apt-get update && apt-get install --yes --no-install-recommends libjemalloc2
RUN apt-get install --yes swi-prolog

# set jemalloc as default malloc in env variable
ENV LD_PRELOAD=/usr/lib/x86_64-linux-gnu/libjemalloc.so.2

# Copy from build stage to keep image size small
COPY --from=build /home/gswb/src/target/GlueSemWorkbench-1.0-SNAPSHOT.jar app.jar

# copy necessary resource files
COPY gswb_resources gswb_resources

## ENTRYPOINT ["java", "-jar", "/app.jar"]
ENTRYPOINT exec java -jar /app.jar -web
