export function generateOTP(otp_length = 0) {
    const digits = '0123456789';
    let OTP = '';
    for (let i = 0; i < otp_length; i++) {
        OTP += digits[Math.floor(Math.random() * 10)];
    }
    return OTP;
}

export function formatTrpcError(trpcError = 'Something went wrong!' as string) {
    if (trpcError?.includes('[\n  {\n  ')) {
      const formattedError = JSON.parse(trpcError);
      const msgError =
        formattedError?.length > 0
          ? formattedError[0].message
          : 'Internal server error';
      return msgError;
    } else {
      return trpcError;
    }
}