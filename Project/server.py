import asyncio
import argparse
import time
import aiohttp
import json
import logging

# google places API key goes here; has been removed for security sake
API_KEY = "KEY"

#server names along with corresponding ports
server_ports = {"Riley": 15560, "Jaquez": 15561, "Juzang": 15562, "Campbell": 15563, "Bernard": 15564}

# servers that each server communicates to
server_communication = {
    "Riley": ["Jaquez", "Juzang"],
    "Jaquez": ["Riley", "Bernard"],
    "Juzang": ["Riley", "Campbell", "Bernard"],
    "Campbell": ["Juzang", "Bernard"],
    "Bernard": ["Jaquez", "Juzang", "Campbell"]
}

# function to check if a string is a number
def is_number(s):
    try:
        float(s)
        return True
    except ValueError:
        return False

class Server:
    def __init__(self, name, ip='127.0.0.1', port=8888, message_max_length=1e6):
        self.name = name
        self.ip = ip
        self.port = port
        self.message_max_length = int(message_max_length)
        self.clients = dict()
        logging.basicConfig(filename=f'{name}.log', encoding='utf-8', format='%(levelname)s:%(message)s', level=logging.INFO)

    # handle inputs from client socket
    async def handle_client_input(self, reader, writer):
        # read messages from client
        data = await reader.read(self.message_max_length)
        message = data.decode()
        addr = writer.get_extra_info('peername')
        logging.info("{} received {} from {}".format(self.name, message, addr))

        # split message by whitespaces
        inputs = message.split()

        # classify message and handle message
        if (len(inputs) != 4):
            # handle AT messages
            if (len(inputs) == 6 and inputs[0] == "AT"):
                # received propogation update about unknown client
                if (inputs[3] not in self.clients):
                    self.clients[inputs[3]] = message
                    await self.propagate_message(self.name, message)

                # update client info from propgated message
                elif (float(inputs[5]) > float(self.clients[inputs[3]].split()[5])):
                    self.clients[inputs[3]] = message
                    await self.propagate_message(self.name, message)
                # already received message, don't propogate
                else:
                    logging.info("received message and not propogating")
                sendback_message = ""
            else:
                sendback_message = f"? {message}"
        else:
            # handle IAMAT message
            if (inputs[0] == "IAMAT"):
                if (self.isValidIMAT(inputs[2], inputs[3])):
                    time_diff = time.time() - float(inputs[3])

                    if (time_diff > 0):
                        time_diff_append = '+'
                    else:
                        time_diff_append = ''

                    sendback_message = f"AT {self.name} {time_diff_append}{time_diff} {inputs[1]} {inputs[2]} {inputs[3]}"
                    self.clients[inputs[1]] = sendback_message

                    #propagate client location
                    await self.propagate_message(self.name, sendback_message)
                else:
                    sendback_message = f"? {message}"

            # handle WHATSAT message
            elif (inputs[0] == "WHATSAT"):
                if (self.isValidWHATSAT(inputs)):
                    client_input = self.clients[inputs[1]]
                    location = client_input.split()[4]
                    radius = inputs[2]
                    bound = inputs[3]
                    places = await self.get_places(location, radius, bound)
                    sendback_message = client_input + '\n' + places.rstrip('\n') + '\n' + '\n'
                else: 
                    sendback_message = f"? {message}"

            # Otherwise invalid message
            else:
                sendback_message = f"? {message}"

        # send back response message
        if (sendback_message != ""):
            logging.info("{} send: {}".format(self.name, sendback_message))
            writer.write(sendback_message.encode())
            await writer.drain()

        logging.info("closed the client socket")
        writer.close()

    # handle requests indefinitely
    async def run_forever(self):
        server = await asyncio.start_server(self.handle_client_input, self.ip, self.port)

        # Serve requests until Ctrl+C is pressed
        logging.info(f'serving on {server.sockets[0].getsockname()}')
        async with server:
            await server.serve_forever()
        # Close the server
        server.close()

    # propogate location updates to neighboring servers
    async def propagate_message(self, server_name, message):
        # open a connection to all neighboring servers and write message
        for servr in server_communication[server_name]:
            try:
                reader, writer = await asyncio.open_connection(self.ip, port= server_ports[servr])
                writer.write(message.encode())
                await writer.drain()
                writer.close()
                await writer.wait_closed()
                logging.info(f"{server_name} propogated {message} to {servr}")
            except:
                logging.info(f"Server {servr} is down, skipping propogation.")

    # asynchronously use Google Places API to get nearby locations given a location and radius
    async def get_places(self, location, radius, bound):
        split_index = location.replace('+', '-').index('-', 1, -1)
        location = location[0:split_index] + ',' + location[split_index:]
        url = 'https://maps.googleapis.com/maps/api/place/nearbysearch/json?key={0}&location={1}&radius={2}'.format(API_KEY, location, radius)

        async with aiohttp.ClientSession(connector=aiohttp.TCPConnector(ssl=False,),) as session: 
            async with session.get(url) as resp:
                # get response from Google
                response = await resp.json()

                # ensure that number of responses is <= bound
                if (len(response["results"]) > int(bound)):
                    response["results"] = response["results"][0:int(bound)]
                    return json.dumps(response, indent=4)
                else:
                    return json.dumps(response, indent=4)
                    

    # check if IMAT message is valid (i.e the latitude, longitude, and timestamp are all valid)
    def isValidIMAT(self, lat_long, time):
        # get latitude and longitude:
        split_index = lat_long.replace('+', '-').index('-', 1, -1)
        latitude = float(lat_long[0:split_index])
        longitude = float(lat_long[split_index:])
        
        # if lat or long are not numbers or lat is not between -90 and 90 and long is not between -180 and 180, return false
        if (((not is_number(latitude)) or (not is_number(longitude))) or (latitude < -90 or latitude > 90) or (longitude < -180 or longitude > 180)):
            return False
        
        # if the time stamp is not a valid number, return false
        if (not is_number(time)):
            return False

        # otherwise, message is valid
        return True

    # chekc if WHATSAT message is valid
    def isValidWHATSAT(self, inputs):
        # return false if client info has not been recieved 
        if (inputs[1] not in self.clients):
            return False

        #ensure that radius is <= 50
        if (not is_number(inputs[2]) or int(inputs[2]) > 50):
            return False

        #ensure that upper bound on Places is <= 20
        if (not is_number(inputs[3]) or int(inputs[3]) > 20):
            return False
        
        return True

# parse CLI args and start up corresponding server
def main():
    #parse command line arguments:
    parser = argparse.ArgumentParser(description = 'Google Places API Proxy Sever')
    parser.add_argument('Name', metavar='[server_name]', type=str, help= 'first and only argument should be a server name.')
    args = parser.parse_args()

    server_name = str(args.Name)

    if (server_name not in server_ports):
        parser.error("Invalid server. Argument must be a valid server name.")

    server = Server(server_name, port=server_ports[server_name])

    # start up server
    try:
        asyncio.run(server.run_forever())

    except KeyboardInterrupt:
        pass

if __name__ == '__main__':
    main()
