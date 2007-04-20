if(window["tercio"]==undefined)
   {
      
      window["tercio"]={
         
         definePackage:function(meta,body){
            var name=meta;
            tokens=name.split(".");
            
            var cur=window;
            
            for(var token in tokens){
               if(!cur[token])
                  {
                     cur[token]={};}
               
               cur=cur[token];}
            
            
            cur.prototype=body;
            
            return cur;
         },
         

         copyPrototype:function(descendant,parent){
            var sConstructor=parent.toString();
            var aMatch=sConstructor.match(/\s*function (.*)\(/);
            if(aMatch!=null)
               {
                  descendant.prototype[aMatch[1]]=parent;
               }
            
            for(var m in parent.prototype)
               {
                  descendant.prototype[m]=parent.prototype[m];
               }
         }
      };
      
      
      
      
      Atom=function(str){
         this.check_arg(str);
         this.core=str;
      };
      

      Atom.prototype={
         core:null,
         
         toTuple : function(){
            return this.core;
         },
         
         check_arg : function(str){
            
            if(/[\s\t\r\n]/.test(str)){
               throw(new Error("Invalid atom"));
            }
         }
      };
      

      Tuple=function(){
         this.arr=new Array();
         for(var i=0;i<arguments.length;i++)
            {
               this.arr.push(arguments[i]);
            }
      };
      

      Tuple.prototype = {
         arr:null,
         
         get:function(index){
            
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


      tercio.definePackage("tercio.msg", {
      
         doCall : function(url) {
            function getXMLHttpRequest() {
               if (window.XMLHttpRequest) 
                  {
                     return new XMLHttpRequest();
                  }
               else if (window.ActiveXObject)
                  {
                     return new ActiveXObject("Microsoft.XMLHTTP");
                  }
            }

            var notifier = NjsRuntime.createNotifier();
            var request = getXMLHttpRequest();
            function ajaxDataReader() {
               if (request.readyState == 4) 
                  {
                     if (request.status == 200 && 
                         request.responseText.length > 0) {
                            notifier();
                         }
                  }
               request.onreadystatechange = ajaxDataReader;
               request.open("post",url, true);
               request.setRequestHeader("Content-Type",
                                        "application/x-www-form-urlencoded;");
               request.send("");
               notifier->wait();
            }
         }
      });
   }




