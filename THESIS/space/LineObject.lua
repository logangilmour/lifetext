LineObject = {}

function LineObject:new(o)
   o = o or {}
   setmetatable(o,self)
   self.__index = self
   self.vertices = {}
   self.edges = {}
   return o
end

function LineObject:addVertex(v)
   table.insert(self.vertices, v)
end

function LineObject:removeVertex(v)
   table.remove(self.vertices, v)
   -- Remove vertex from edge list
end

function LineObject:addEdge(v1, v2) --?????
   local row = edges[v1] 
   if row ~= nil then table.remove(row,v2) end
   
   local column = edges[
end

