if (!tercio) 
   {
      window.tercio = {
         
         PackageClass : Class.create(),
         Package : function(meta, body) {
            return new PackageClass(body)
         }

      };
      
      
      tercio.PackageClass.prototype = {
         
         /**
          * Initialize a new package. 
          * Create the package namespace and then
          * apply the prototype at the lowest 
          * level of the package
          */
         initialize : function(body) {
            var name = meta.namespace
            tokens = name.split(".")
            
            var cur = window
            
            tokens.each(
                        function(token) {
                           if (!cur[token])
                              {
                                 cur[token] = {}
                              }
                           cur = cur[token]
                        })
            
            cur.prototype = body
            
            return cur
         }
         
      }
      
   }






