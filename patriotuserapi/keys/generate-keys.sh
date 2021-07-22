#!/bin/sh

openssl req -newkey rsa:4096 -nodes -sha512 -x509 -days 3650 -nodes -out server-cert.pem -keyout server-key.pem -subj "/C=BE/ST=Ottignies/L=Louvain-la-Neuve/O=UCLouvain/CN=patriot"


