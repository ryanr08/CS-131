import asyncio
import argparse

servers = {"Riley": 15560, "Jaquez": 15561, "Juzang": 15562, "Campbell": 15563, "Bernard": 15564}

class Client:
    def __init__(self, ip='127.0.0.1', port=8888, name='client', message_max_length=1e6):
        self.ip = ip
        self.port = port
        self.name = name
        self.message_max_length = int(message_max_length)

    async def tcp_echo_client(self, message):
        reader, writer = await asyncio.open_connection(self.ip, self.port)
        print(f'{self.name} send: {message}')
        writer.write(message.encode())

        data = await reader.read(self.message_max_length)
        print(f'{self.name} received: {data.decode()}')

        print('close the socket')
        writer.close()

    def run_until_quit(self):
        # start the loop
        while True:
            # collect the message to send
            message = input("Please input the next message to send: ")
            if message in ['quit', 'exit', ':q', 'exit;', 'quit;', 'exit()', '(exit)']:
                break
            else:
                asyncio.run(self.tcp_echo_client(message))

def main():
    #parse command line arguments:
    parser = argparse.ArgumentParser(description = 'Google Places API Proxy Sever')
    parser.add_argument('Name', metavar='[server_name]', type=str, help= 'first and only argument should be a server name.')
    args = parser.parse_args()
    
    server_name = str(args.Name)

    if (server_name not in servers):
        parser.error("Invalid server. Argument must be a valid server name.")

    client = Client(port= servers[server_name])
    client.run_until_quit()

if __name__ == '__main__':
    main()