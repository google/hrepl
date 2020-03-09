"""A macro for testing hrepl."""

_hrepl_test_binary = "//hrepl/tests:hrepl_test_binary"

def _quote(s):
    """Wraps the given string in quotes.

    The "args" of sh_test are subject to shell tokenization.
    This function wraps it in '...' so Bazel will treat it as a single
    argument.

    Args:
      s: A string.

    Returns:
      The string surrounded by quotes.
    """

    # TODO(judahjacobson): This would cause ambiguity if we ever
    # started using "'" in a script.  So far, we haven't needed it.
    return "'" + s + "'"

def _hrepl_test_impl(ctx):
    output = ctx.actions.declare_file(ctx.label.name)
    ctx.actions.write(
        output,
        "#!/bin/bash\nexec $RUNFILES_DIR/{}/{} {}".format(
            "hrepl",  # TODO: how to get this automatically?
            ctx.executable._test.short_path,
            " ".join([
                _quote(q)
                for q in ([
                    "--script",
                    "\n".join(ctx.attr.commands),
                    "--expected",
                    ctx.attr.expected_output,
                ] + ["--arg=" + a for a in ctx.attr.test_args])
            ])),
        is_executable = True)
    return DefaultInfo(
        executable = output,
        runfiles = ctx.runfiles(
            files=[ctx.executable._test]).merge(
            ctx.attr._test[DefaultInfo].default_runfiles))


_hrepl_test = rule(
    test = True,
    implementation = _hrepl_test_impl,
    attrs = {
        "test_args": attr.string_list(),
        "commands": attr.string_list(),
        "expected_output": attr.string(),
        "_test": attr.label(
            executable = True,
            cfg = "target",
            default = Label(_hrepl_test_binary))
    }
)

def hrepl_test(name="", test_args=[], commands=[], expected_output="", tags=[], **kwargs):
    """Declares a test of the hrepl binary.

    Args:
      name: The name of this rule.
      test_args: List of flags to pass to hrepl.
      commands: List of commands to run in the hrepl REPL.  The substring
        "{OUT}" will be replaced by the path to the output file.
      expected_output: Expected vvalue for the output file.
      tags: Tags for this rule.
      **kwargs: Other rule parameters.
    """
    _hrepl_test(
        name = name,
        test_args = test_args,
        commands = commands,
        expected_output = expected_output,
        tags = tags + ["hrepl_test"],
        **kwargs
    )
