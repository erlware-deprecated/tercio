alert("Hello2");
if (window["tercio"] == undefined) 
   {

      window["tercio"] = {
         
         PackageClass : Base.extend({         
           /**            
            * Initialize a new package. 
            * Create the package namespace and then
            * apply the prototype at the lowest 
            * level of the package
            */
            constructor : function(body) {
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
         }),


         Package : function(meta, body) {
            return new PackageClass(body);
         }

      };


      Atom = Base("");
      Atom.extend({
         toTuple : function() {
            return this;
         }
      });

      Tuple = Base.extend({
         arr : null,

         constructor : function() {
            this.arr = new Array(arguments);
         },
         
         get : function(index) {

            return this.arr[index];

         },


         toTuple : function() {
            
            var tmp = this.collect(function(value) {
               return value.toTuple(); 
            });
            
            return "{" + tmp.join(", ") + "}";
            
         }
      });

      Object.extend(Array.prototype, {
         toTuple : function() {
            
            var tmp = this.collect(function(value) {
               return value.toTuple(); 
            });
            
            return "[" + tmp.join(", ") + "]";
         }
      });
      
      Object.extend(Number.prototype, {
         toTuple : function () {
            return isFinite(this) ? String(this) : "null";
         }
      });

      Object.extend(Boolean.prototype, {
         toTuple : function () {
            return String(this);
         }
      });
 
      Object.extend(String.prototype, {
         toTuple : function () {
            if (/["\\\x00-\x1f]/.test(this)) {
                return '"' + this.replace(/([\x00-\x1f\\"])/g, function(a, b) {
                    var c = m[b];
                    if (c) {
                        return c;
                    }
                    c = b.charCodeAt();
                    return '\\u00' +
                        Math.floor(c / 16).toString(16) +
                        (c % 16).toString(16);
                }) + '"';
            }
            return '"' + this + '"';
        }
      });
      












   }






