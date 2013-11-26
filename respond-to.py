#!/usr/bin/python

from SimpleHTTPServer import SimpleHTTPRequestHandler
from BaseHTTPServer import HTTPServer
import time
import sys
import getopt

##globals to use in the http response
##file location
response_txt = ''
##http response delay
reponse_delay=''
##http response code
response_code=''

##http request handler
class MyRequestHandler(SimpleHTTPRequestHandler) :
	def handleRequest(self):
		print('received request message')
		length = int(self.headers.get('content-length'))
		requestBody = self.rfile.read(length)
		print(requestBody)

		responseFile = open(response_txt)
		response = responseFile.read()
		time.sleep(response_delay)
		self.send_response(response_code)
                self.send_header("Content-type", "application/xml")
                self.send_header("charset", "UTF-8")
		self.end_headers()
		self.wfile.write(response)
		self.wfile.close()

	def do_POST(self):
		self.handleRequest()
	
def usage():
	print("Usage: %s -h host -p port -d response_delay -c http_response_code -v response_txt_file_location" % sys.argv[0])
        sys.exit(2)


		
def main():
	try:
		global response_txt, response_delay, response_code
		host=''
		port=''
		total = len(sys.argv)
		if total != 11:
			usage() 
		try:
	    		myopts, args = getopt.getopt(sys.argv[1:],"h:p:d:c:v:")
			for o, a in myopts:
                    		if o == '-h':
	        	                host=a
        	        	elif o == '-p':
		                        port=int(a)
	        	        elif o == '-v':
        	        	        response_txt=a
				elif o == '-d':
					response_delay=int(a)
				elif o == '-c':
					response_code=int(a)
				else:
					usage()

		except getopt.GetoptError:
			usage()
	
		print('ResponseTo is listening on %s:%d.'%(host, port))
		print('HTTP response code will be %d' % response_code)
		print('HTTP response delay will be %d secs' % response_delay)
		print('HTTP response will contain the contents of %s' % response_txt)
		server = HTTPServer((host, port), MyRequestHandler)
		server.serve_forever()
	except KeyboardInterrupt:
		print ('Shutting down')
		server.socket.close()

if __name__ == '__main__':
	 main()
