%%% @author vlad <lib.aca55a@gmail.com>
%%% @copyright (C) 2014, vlad
%%% @doc
%%%
%%% @end
%%% Created : 20 Sep 2014 by vlad <lib.aca55a@gmail.com>

-ifndef(mme_hrl).
-define(mme_hrl, true).

-define(D_S1AP_MAX_INC_PARA, 100).

-record(mme_header,
        { msg_length,  %%  16-bit unsigned
          dest_id,     %%  8-bit unsigned 
          msg_type,    %%  8-bit unsigned 
          msg_id1,     %%  8-bit unsigned 
          msg_id2,     %%  8-bit unsigned 
          llid,        %%  8-bit unsigned 
          status,      %%  8-bit unsigned 
          time,        %%  16-bit unsigned
          err_info,    %%  8-bit unsigned 
          dummy,       %%  8-bit unsigned 
          msgDiscr,    %%  8-bit unsigned 
          msgType,     %%  8-bit unsigned 
          chanNumbIEI, %%  8-bit unsigned 
          chanNumb,    %%  8-bit unsigned 
          aI_parameter %%  UINT8         aI_parameter[D_S1AP_MAX_INC_PARA];
        }
       ).

-record(mme_packet,
        {header,     %% #mme_header{}
         avps,       %% deep list() of #mme_avp{}
         msg,        %% fully decoded message
         bin,        %% binary received/sent over the wire
         errors = [],%% list() of Result-Code | {Result-Code, #mme_avp{}}
         transport_data}).

-record(diameter_service,
        {pid,
         host_ip_address = []      %% ['Address'()]
        }).

-endif. %% -ifdef(mme_hrl).


