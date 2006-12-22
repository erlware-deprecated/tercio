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
