% This record characterises the connection from the browser to our
% server it is intended to be a consistent view derived from a bunch
% of different headers
-record(req, {sock=none,                  % The associated socket       
              connection=keep_alive,	  % keep_alive | close
              content_length,                 % Integer
              vsn,                            % {Maj,Min}
              method,                         % 'GET'|'POST'
              uri,				  % URI /index.html
              args=[],                        % Part of URI after ?
              headers,			  % [{Tag, Val}]
              body = <<>>}).		  % Content Body

-define(not_implemented_501, "HTTP/1.1 501 Not Implemented\r\n\r\n").
-define(forbidden_403, "HTTP/1.1 403 Forbidden\r\n\r\n").
-define(not_found_404, "HTTP/1.1 404 Not Found\r\n\r\n").
