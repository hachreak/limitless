#!/bin/sh

MONGODB=3.4.3

wget http://fastdl.mongodb.org/linux/mongodb-linux-x86_64-${MONGODB}.tgz
tar xzf mongodb-linux-x86_64-${MONGODB}.tgz
${PWD}/mongodb-linux-x86_64-${MONGODB}/bin/mongod --version
mkdir ${PWD}/mongodb-linux-x86_64-${MONGODB}/data
${PWD}/mongodb-linux-x86_64-${MONGODB}/bin/mongod --dbpath ${PWD}/mongodb-linux-x86_64-${MONGODB}/data --logpath ${PWD}/mongodb-linux-x86_64-${MONGODB}/mongodb.log --fork
