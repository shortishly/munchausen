FROM scratch

ARG REL_NAME
ARG REL_VSN=1
ARG ERTS_VSN

ENV BINDIR /erts-7.3/bin
ENV BOOT /releases/1/munchausen_release
ENV CONFIG /releases/1/sys.config
ENV ARGS_FILE /releases/1/vm.args

ENV TZ=GMT

ENTRYPOINT exec ${BINDIR}/erlexec -boot_var /lib -boot ${BOOT} -noinput -config ${CONFIG} -args_file ${ARGS_FILE}

ADD _rel/munchausen_release/ /
