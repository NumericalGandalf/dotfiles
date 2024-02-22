local M = {}

function M.setup()
  require("gandalf.settings").setup()
  require("gandalf.keymaps").setup()
  require("gandalf.lazy").setup()
end

return M
