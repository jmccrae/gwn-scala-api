FROM tomcat:9.0-jdk8-openjdk
MAINTAINER john@mccr.ae

EXPOSE 8080

ADD target/scala-2.12/gwn-scala-api-web_2.12-0.2.war /usr/local/tomcat/
RUN mkdir /usr/local/tomcat/webapps/gwn-scala-api
RUN unzip /usr/local/tomcat/gwn-scala-api-web_2.12-0.2.war -d /usr/local/tomcat/webapps/gwn-scala-api
