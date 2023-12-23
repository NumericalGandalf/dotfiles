return {
    'pocco81/auto-save.nvim',
    event = 'BufEnter',
    config = function()
        vim.api.nvim_create_autocmd("BufWritePre", {
            callback = function()
                vim.lsp.buf.format { async = false }
            end
        })
        require("auto-save").setup({
            enabled = true,
            execution_message = {
                message = function()
                    return ("AutoSave: saved buffer at " .. vim.fn.strftime("%H:%M:%S"))
                end,
                dim = 0.2,
                cleaning_interval = 1000,
            },
            trigger_events = { "InsertLeave", "TextChanged" },
            condition = function(buf)
                local utils = require("auto-save.utils.data")
                if
                    vim.fn.getbufvar(buf, "&modifiable") == 1 and utils.not_in(vim.fn.getbufvar(buf, "&filetype"), {}) then
                    return true
                end
                return false
            end,
            write_all_buffers = false,
            debounce_delay = 135,
            callbacks = {
                enabling = nil,
                disabling = nil,
                before_asserting_save = nil,
                before_saving = nil,
                after_saving = nil
            }
        })
    end
}
