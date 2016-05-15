/* A simple server in the internet domain using TCP
   The port number is passed as an argument */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>

void error(const char *msg)
{
  perror(msg);
  exit(1);
}

/* do_socket() creates a new AF_UNIX socket */
int do_socket(void) {
  return socket(AF_INET, SOCK_STREAM, 0);
}

int create_tcp_server(int portno)
{
  int sockfd, newsockfd;
  socklen_t clilen;
  char buffer[256];
  struct sockaddr_in serv_addr, cli_addr;
  int n;

  //sockfd = socket(AF_INET, SOCK_STREAM, 0);
  sockfd = do_socket();
  if (sockfd < 0)
    error("ERROR opening socket");
  bzero((char *) &serv_addr, sizeof(serv_addr));
  //portno = atoi(argv[1]);
  serv_addr.sin_family = AF_INET;
  serv_addr.sin_addr.s_addr = INADDR_ANY;
  serv_addr.sin_port = htons(portno);
  if (bind(sockfd, (struct sockaddr *) &serv_addr,
           sizeof(serv_addr)) < 0)
    error("ERROR on binding");
  listen(sockfd,5);
  clilen = sizeof(cli_addr);
  newsockfd = accept(sockfd,
                     (struct sockaddr *) &cli_addr,
                     &clilen);
  if (newsockfd < 0)
    error("ERROR on accept");
  bzero(buffer,256);
  n = read(newsockfd,buffer,255);
  if (n < 0) error("ERROR reading from socket");
  printf("Here is the message: %s\n",buffer);
  n = write(newsockfd,"I got your message",18);
  if (n < 0) error("ERROR writing to socket");
  close(newsockfd);
  close(sockfd);
  return 0;
}

int create_tcp_client(char *host, int portno)
{
  int sockfd, n;
  struct sockaddr_in serv_addr;
  struct hostent *server;

  char buffer[256];

  //sockfd = socket(AF_INET, SOCK_STREAM, 0);
  sockfd = do_socket();
  if (sockfd < 0)
    error("ERROR opening socket");
  server = gethostbyname(host);
  if (server == NULL) {
    fprintf(stderr,"ERROR, no such host\n");
    exit(0);
  }
  bzero((char *) &serv_addr, sizeof(serv_addr));
  serv_addr.sin_family = AF_INET;
  bcopy((char *)server->h_addr,
        (char *)&serv_addr.sin_addr.s_addr,
        server->h_length);
  serv_addr.sin_port = htons(portno);
  if (connect(sockfd,(struct sockaddr *) &serv_addr,sizeof(serv_addr)) < 0)
    error("ERROR connecting");
  printf("Please enter the message: ");
  bzero(buffer,256);
  fgets(buffer,255,stdin);
  n = write(sockfd,buffer,strlen(buffer));
  if (n < 0)
    error("ERROR writing to socket");
  bzero(buffer,256);
  n = read(sockfd,buffer,255);
  if (n < 0)
    error("ERROR reading from socket");
  printf("%s\n",buffer);
  close(sockfd);
  return 0;
}
