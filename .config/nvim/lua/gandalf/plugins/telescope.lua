local M = { "nvim-telescope/telescope.nvim" }

M.dependencies = {
  "nvim-lua/plenary.nvim",
  {
    "nvim-telescope/telescope-fzf-native.nvim",
    build = "make",
  },
}

M.event = "VeryLazy"

function M.config()
  local telescope = require("telescope")
  local builtin = require("telescope.builtin")
  local actions = require("telescope.actions")

  telescope.setup({
    defaults = {
      mappings = {
        i = {
          ["<C-g>"] = actions.close,
        },
      },
    },
    pickers = {
      find_files = {
        find_command = { "fd", "-t", "f", "--strip-cwd-prefix" },
        hidden = true,
        follow = true,
      },
      grep_string = {
        only_sort_text = true,
      },
      planets = {
        show_pluto = true,
        show_moon = true,
      },
    },
    extensions = {
      fzf = {
        fuzzy = true,
        override_generic_sorter = true,
        override_file_sorter = true,
        case_mode = "smart_case",
      },
    },
  })

  telescope.load_extension("fzf")

  vim.keymap.set("n", "<leader>ff", builtin.find_files)
  vim.keymap.set("n", "<leader>fb", builtin.buffers)

  vim.keymap.set("n", "<leader>fg", builtin.live_grep)
  vim.keymap.set("n", "<leader>fG", builtin.current_buffer_fuzzy_find)
  vim.keymap.set("n", "<leader>fs", builtin.grep_string)

  vim.keymap.set("n", "<leader>fc", builtin.git_status)
end

return M
