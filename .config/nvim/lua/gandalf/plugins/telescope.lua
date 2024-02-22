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
          ["<C-f>"] = actions.preview_scrolling_down,
          ["<C-b>"] = actions.preview_scrolling_up,
          ["<C-u>"] = function()
            vim.api.nvim_command("%delete")
          end,
        },
      },
    },
    pickers = {
      find_files = {
        find_command = {
          "fd",
          "--strip-cwd-prefix",
          "--hidden",
          "--type",
          "file",
        },
        hidden = true,
        follow = true,
      },
      live_grep = {
        additional_args = function()
          return {
            "--hidden",
            "--follow",
            "--smart-case",
            "-g!.git/",
          }
        end,
      },
      grep_string = {
        only_sort_text = true,
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

  local opts = { noremap = true }

  vim.keymap.set("n", "<leader>ff", builtin.find_files, opts)
  vim.keymap.set("n", "<leader>fb", builtin.buffers, opts)

  vim.keymap.set("n", "<leader>fg", builtin.live_grep, opts)
  vim.keymap.set("n", "<leader>fG", builtin.current_buffer_fuzzy_find, opts)
  vim.keymap.set("n", "<leader>fs", builtin.grep_string, opts)

  vim.keymap.set("n", "<leader>fc", builtin.git_status, opts)
end

return M
