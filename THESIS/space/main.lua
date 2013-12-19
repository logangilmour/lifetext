require("Camera")
require("Level")
require("Tile")


camera = Camera:new()

level = Level:new()

function love.load()
end

function love.update(dt)

   camera:update()
   level:update()
end

function love.draw()
   camera:set()
   level:render()
   camera:unset()
end

function love.keypressed(key)   -- we do not need the unicode, so we can leave it out
   if key == "escape" then
      love.event.push("quit")   -- actually causes the app to quit
   end
end
