if(window["tercio"]==undefined)
   {
      
      window["tercio"]={
         
         callUrl : "/tercio/call",
         castUrl : "/tercio/cast",

         definePackage:function(meta,body){
            var name=meta;
            var tokens = name.split(".");
            
            var cur=window;
            
            tokens.each(function(token) {
               if(!cur[token])
                  {
                     cur[token] = {};
                  }
               
               cur = cur[token];
            });
            
            Object.extend(cur, body);
            
            return cur;
         },

         defineClass : function(body) {
            var nClass = Class.create();
            nClass.prototype = body;
            return nClass;
         },

         resolveTemplate : function(name) {
            var host = document.location.host;
            var protocol = document.location.protocol;
            
            return protocol + "//" + host + "/tercio/" + name; 

         }
      };
      
      Atom = Class.create();
      Atom.prototype = {
         core:null,
         
         initialize: function() {
            check_arg(str);
            this.core=str;
         },
     
     
         
         toTuple : function(){
            return this.core;
         },
         
         check_arg : function(str){
            
            if(/[\s\t\r\n]/.test(str)){
               throw(new Error("Invalid atom"));
            }
         }
      };
      

      Tuple = Class.create();
      Tuple.prototype = {

         arr:null,

         initialize : function() {
            if (arguments.length == 1 && arguments[0].isArray &&
                arguments[0].isArray()) 
               {
                  this.arr = arguments[0];
               } else {
                  this.arr= $A(arguments);
               }
         },
      
         
         get : function(index){
            return this.arr[index];
         },

         toTuple : function(){


            var len=this.arr.length;
            var cstop=this.arr.length-1;
            var tmp="{";
            
            for(var i=0;i<len;i++)
               {
                  tmp+=this.arr[i].toTuple();
                  if(i<cstop)
                     {
                        tmp+=",";
                     }
               }
            return tmp+"}";
         }
      };

      Array.prototype.toTuple=function(){
         
         var len=this.length;
         var cstop=this.length-1;
         
         var tmp="[";
         
         for(var i=0;i<len;i++)
            {
               tmp+=this[i].toTuple();
               
               if(i<cstop)
                  {
                     tmp+=",";
                  }
            }
         
         return tmp+"]";
      };

      Array.prototype.isArray = function() {
         return true;
      };


      Number.prototype.toTuple = function() {
         return isFinite(this) ? String(this) : "null";
      };


      Boolean.prototype.toTuple=function() {
         return String(this);
      };
      
      
      String.prototype.toTuple = function() {
         if(/[\"\\\x00-\x1f]/.test(this)) {
            return "\""+
            this.replace(/([\x00-\x1f\\\"])/g,
                         function(a,b) {
                            var c=m[b];
                            if(c) {
                               return c;
                            }

                            c=b.charCodeAt();
                            return "\\u00"+
                            Math.floor(c/16).toString(16) +
                               (c%16).toString(16);})
               +"\"";
         }

         return "\""+this+"\"";
      };


      tercio.definePackage("tercio.tmpl", {
         
         templateHash : new Array(),

         registerTemplate : function(name, templateSource) {
            tercio.tmpl.templateHash[name] = Trimpath.parseTemplate(templateSource);
         },

         templateExists : function(name) {
            return tercio.tmpl.templateHash[name] != undefined;
         },

         template : function (name) {
            return tercio.tmpl.templateHash[name];
         }
      });



      tercio.definePackage("tercio.msg", {
         Result : tercio.defineClass({
            merge : function(templateName) {
               this.templateUrl = tercio.resolveTemplate(templateName);
               this.templateName = templateName;
               new Ajax.request(this.templateUrl,
                                {
                                   method:'get',
                                   onSuccess: function(transport){
                                      var response = transport.responseText || "ERROR_NO_TEMPLATE";
                                      tercio.tmpl.registerTemplate(templateName, response);
                                   },
                                   
                                   onFailure: function(){ 
                                      tercio.tmpl.registerTemplate(templateName, "ERROR_INVALID_TEMPLATE");
                                   }
                                });
            },

            into : function(domElementName) {
               this.element = $(domElementName);
            },
            
            castComplete : function() {
               // Nothing to do here
            },

            castFailure : function() {
               // Nothing to do here
            },

            callComplete : function(data) {
               var element = $(this.domElementName);
               element.innerHTML = tercio.tmpl.template(this.templateName).process(data);
            },

            callFailure : function() {
               // do something useful here
            }
            
         }),


         Message : tercio.defineClass({
            message : null,

            result : null,

            initialize : function(msg) {
               this.message = msg;
               this.result = new tercio.msg.Result(this);
            },

            to : function(target) {
               this.target = target;
               return this;
            },
            
            result : function() {
               return this.result;
            },

            cast : function() {
               var obj = this;
               new Ajax.request(tercio.castUrl,
                                {
                                   method : 'post',
                                   paramenters : $H({message_contents : obj.msg}),
                                   onSuccess : function(transport){
                                      result.castComplete();
                                   },
                                   
                                   onFailure : function(){ 
                                      result.castFailure();
                                   }
                                });
            },

            call : function() {
               var obj = this;
               new Ajax.request(tercio.callUrl,
                                {
                                   method: 'post',
                                   parameters: $H({message_contents : obj.msg}),
                                   onSuccess: function(transport){
                                      if (transport.responseText) {
                                         try {
                                            var data = eval('(' + transport.responseText + ')');
                                            obj.result.callComplete(data);
                                         } catch (err) {
                                            obj.result.callFailure(transport.responseText, err);
                                         }
                                      }
                                   },
                                   
                                   onFailure: function(){ 
                                      obj.result.callFailure();
                                   }
                                });
            }


            
            
         }),
         

         
      });
   }




