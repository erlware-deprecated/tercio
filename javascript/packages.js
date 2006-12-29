if (!core) 
   {
      window.core = {
         Package : Class.create()
         Loader : Class.create()
      }
      
      
      core.Loader.prototype = {


      }



      
      core.Package.prototype = {
         
         loading : []
                  
         /**
          * Initialize a new package. 
          * Create the package namespace and then
          * apply the prototype at the lowest 
          * level of the package
          */
         initialize : function(name, proto) {
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

            cur.prototype = proto

            return cur
         },
         
         load : function(name) {
            tokens = name.split(".")
            location = "/package/" + tokens.join("/") + ".js"
            
            
            // We only want to load a package once so
            // lets make sure that the namespace doesn't already exist.
            
            var loaded = core.Package.loaded.dectect(
                                                     function(lib) {
                                                        lib == name
                                                     })
            
            if (!loaded) 
               {
                  var head = document.getElementsByTagName("head")[0]
                  var script = document.createElement('script')
                  script.id = name
                  script.type = 'text/javascript'
                  script.src = location
                  head.appendChild(script)
                  core.Package.loading.push(name)
               }
         }

         alias : function(newName, oldName) {
            
         }
         
      }

   }






