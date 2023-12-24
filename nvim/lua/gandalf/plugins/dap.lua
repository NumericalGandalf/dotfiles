return {
    {
        'mfussenegger/nvim-dap',
        config = function()
            local dap = require("dap")
            vim.keymap.set('n', '<F5>', function() dap.continue() end)
            vim.keymap.set('n', '<F6>', function() dap.restart() end)
            vim.keymap.set('n', '<F7>', function() dap.terminate() end)
            vim.keymap.set('n', '<F10>', function() dap.step_over() end)
            vim.keymap.set('n', '<F11>', function() dap.step_into() end)
            vim.keymap.set('n', '<F12>', function() dap.step_out() end)
            vim.keymap.set('n', '<F9>', function() dap.toggle_breakpoint() end)
            vim.keymap.set('n', '<F4>', function() dap.repl.toggle() end)
        end
    },
    {
        "rcarriga/nvim-dap-ui",
        dependencies = { "mfussenegger/nvim-dap" },
        config = function()
            local dap = require("dap")
            local dapui = require("dapui")
            local widgets = require("dap.ui.widgets")
            vim.keymap.set({ 'n', 'v' }, '<Leader>dh', function() dapui.float_element() end)
            vim.keymap.set({ 'n', 'v' }, '<Leader>dv', function() dapui.eval() end)
            vim.keymap.set('n', '<Leader>df', function() widgets.centered_float(widgets.frames) end)
            vim.keymap.set('n', '<Leader>ds', function() widgets.centered_float(widgets.scopes) end)
            dapui.setup({
                controls = {
                    element = "repl",
                    enabled = false,
                    icons = {
                        disconnect = "",
                        pause = "",
                        play = "",
                        run_last = "",
                        step_back = "",
                        step_into = "",
                        step_out = "",
                        step_over = "",
                        terminate = ""
                    }
                },
                element_mappings = {},
                expand_lines = true,
                floating = {
                    border = "single",
                    mappings = {
                        close = { "q", "<Esc>" }
                    }
                },
                force_buffers = true,
                icons = {
                    collapsed = "",
                    current_frame = "",
                    expanded = ""
                },
                layouts = { {
                    elements = { {
                        id = "breakpoints",
                        size = 0.2
                    }, {
                        id = "stacks",
                        size = 0.2
                    }, {
                        id = "watches",
                        size = 0.3
                    }, {
                        id = "scopes",
                        size = 0.3
                    }, },
                    position = "left",
                    size = 35
                }, {
                    elements = { {
                        id = "repl",
                        size = 1
                    }, },
                    position = "bottom",
                    size = 13
                } },
                mappings = {
                    edit = "e",
                    expand = { "<CR>", "<2-LeftMouse>" },
                    open = "o",
                    remove = "d",
                    repl = "r",
                    toggle = "t"
                },
                render = {
                    indent = 1,
                    max_value_lines = 100
                }
            })
            dap.listeners.after.event_initialized["dapui_config"] = function() dapui.open() end
            dap.listeners.before.event_terminated["dapui_config"] = function() dapui.close() end
            dap.listeners.before.event_exited["dapui_config"] = function() dapui.close() end
        end
    }
}
