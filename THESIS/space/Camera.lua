Camera = {}

function Camera:new(o)
   o = o or {}
   setmetatable(o,self)
   self.__index = self

   self.x = 0
   self.y = 0
   self.scaleX = 1
   self.scaleY = 1
   self.rotation = 0
   return o
end

function Camera:set()
  love.graphics.push()
  love.graphics.rotate(-self.rotation)
  love.graphics.scale(1 / self.scaleX, 1 / self.scaleY)
  love.graphics.translate(-self.x, -self.y)
end

function Camera:unset()
  love.graphics.pop()
end

function Camera:move(dx, dy)
  self.x = self.x + (dx or 0)
  self.y = self.y + (dy or 0)
end

function Camera:rotate(dr)
  self.rotation = self.rotation + dr
end

function Camera:scale(sx, sy)
  sx = sx or 1
  self.scaleX = self.scaleX * sx
  self.scaleY = self.scaleY * (sy or sx)
end

function Camera:setPosition(x, y)
  self.x = x or self.x
  self.y = y or self.y
end

function Camera:setScale(sx, sy)
  self.scaleX = sx or self.scaleX
  self.scaleY = sy or self.scaleY
end

function Camera:update()
   if love.keyboard.isDown(" ") then
      local deltaX, deltaY = love.mouse.getPosition()
      self:move(self.oldMouseX-deltaX,self.oldMouseY-deltaY)
      love.mouse.setPosition(self.oldMouseX,self.oldMouseY)
   end
   
   self.oldMouseX, self.oldMouseY = love.mouse.getPosition()
end

function Camera:mousePosition()
  return love.mouse.getX() * self.scaleX + self.x, love.mouse.getY() * self.scaleY + self.y
end
