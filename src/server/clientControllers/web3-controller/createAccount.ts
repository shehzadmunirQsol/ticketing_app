import { ethers } from 'ethers';
import { createSmartAccountClient } from '@biconomy/account';

type CreateSmartAccountType = {
  private_address?: string;
};

/**
 * Creates a smart account using the provided private address or a default one if not provided.
 * @param props An object containing the private address for creating the smart account.
 * @returns A smart account object.
 */
export const createSmartAccount = async (
  props: CreateSmartAccountType = {},
): Promise<any> => {
  try {
    // Initialize provider with RPC URL
    const provider = new ethers.providers.JsonRpcProvider(process.env.RPC_URL);

    // Initialize signer with private address or default to empty string
    const signer = new ethers.Wallet(props?.private_address ?? '', provider);

    // Configure smart account options
    const biconomySmartAccountConfig: any = {
      signer: signer,
      bundlerUrl: process.env.BUNDLER_URL, // Read about this at https://docs.biconomy.io/dashboard#bundler-url
      biconomyPaymasterApiKey: process.env.PAYMASTER_API_KEY, // Read about at https://docs.biconomy.io/dashboard/paymaster
    };

    // Create smart account client
    const smartAccount = await createSmartAccountClient(
      biconomySmartAccountConfig,
    );

    return smartAccount;
  } catch (error: any) {
    // Throw error if creation fails
    throw new Error(error?.message);
  }
};
