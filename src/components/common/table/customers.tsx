import React, { useMemo, useState } from 'react';
import {
  ColumnDef,
  SortingState,
  VisibilityState,
  flexRender,
  getCoreRowModel,
  getFilteredRowModel,
  getPaginationRowModel,
  getSortedRowModel,
  useReactTable,
} from '@tanstack/react-table';
import { ChevronDown, MoreHorizontal } from 'lucide-react';
import { ScrollArea, ScrollBar } from '~/components/ui/scroll-area';
import { Button } from '@/ui/button';
import {
  DropdownMenu,
  DropdownMenuCheckboxItem,
  DropdownMenuContent,
  DropdownMenuItem,
  DropdownMenuLabel,
  DropdownMenuSeparator,
  DropdownMenuTrigger,
} from '@/ui/dropdown-menu';
import {
  Table,
  TableBody,
  TableCell,
  TableHead,
  TableHeader,
  TableRow,
} from '@/ui/table';
import { trpc } from '~/utils/trpc';
import { customEmailTruncateHandler, displayDate } from '~/utils/helper';
import { getCustomerSchema } from '~/schema/customer';
import {
  Tooltip,
  TooltipContent,
  TooltipProvider,
  TooltipTrigger,
} from '~/components/ui/tooltip';
import { Switch } from '~/components/ui/switch';
import { CustomerDialog } from '../modal/customers';
import { useToast } from '~/components/ui/use-toast';
import { LoadingDialog } from '../modal/loadingModal';

import { TableFilters } from './table_filters';
import {
  Select,
  SelectContent,
  SelectItem,
  SelectTrigger,
  SelectValue,
} from '~/components/ui/select';
import {
  DoubleArrowLeftIcon,
  DoubleArrowRightIcon,
} from '@radix-ui/react-icons';
import { ChevronLeftIcon, ChevronRightIcon } from '@radix-ui/react-icons';
import { useRouter } from 'next/router';
import { CSVLink } from 'react-csv';

export type CustomerType = {
  email: string;
  phone_number: string;
  username: string | null;
  first_name: string;
  last_name: string;
  is_approved: boolean;
  is_disabled: boolean;
  is_blocked: boolean;
  is_verified: boolean;
  id: number;
  dob: Date;
  created_at: Date;
  updated_at: Date;
};

const initialFilters: any = {
  first: 0,
  rows: 10,
};

export default function CustomersDataTable() {
  // use toast
  const { toast } = useToast();
  const router = useRouter();
  const { is_verified } = router.query;
  const [sorting, setSorting] = useState<SortingState>([]);
  const [filterID, setFilterID] = useState({});

  const [filters, setFilters] = useState<getCustomerSchema>(initialFilters);
  const [columnVisibility, setColumnVisibility] = useState<VisibilityState>({});
  const [rowSelection, setRowSelection] = useState({});
  const [selectedItem, setSelectedItem] = useState({});
  const [title, setTitle] = useState('');
  const [type, setType] = useState('');
  const [isModal, setIsModal] = useState(false);
  const [para, setPara] = useState('');

  // APi
  const { data, refetch, isLoading } = trpc.customer.getCustomers.useQuery(
    { ...filters, filters: { ...filterID } },
    {
      refetchOnWindowFocus: false,
    },
  );

  const customerData = useMemo(() => {
    return Array.isArray(data?.data) && data?.data?.length ? data?.data : [];
  }, [data]);

  const deleteCustomer: any = trpc.customer.update.useMutation({
    onSuccess: () => {
      console.log('updated successfully');
    },
    onError(error: any) {
      console.log({ error });
    },
  });

  const deleteUser = (data: any, type: string) => {
    setSelectedItem(data);
    setTitle('Customer');
    {
      type == 'delete'
        ? setPara('Are you sure you want to Delete this customer?')
        : type == 'enable'
        ? setPara('Are you sure you want to Enable this customer?')
        : type == 'disable'
        ? setPara('Are you sure you want to Disable this customer?')
        : type == 'block'
        ? setPara(
            `Are you sure you want to ${
              data.is_blocked ? 'Unblock' : 'Block'
            } this customer?`,
          )
        : setPara('');
    }
    setType(type);
    setIsModal(true);
  };

  const displayFirstName = (data: any) => {
    if (data.is_disabled) {
      return (
        <div className="flex gap-2 items-center">
          <p className="text-ellipsis text-left whitespace-nowrap overflow-hidden w-fit  text-white">
            {data.first_name}
          </p>

          <TooltipProvider>
            <Tooltip>
              <TooltipTrigger>
                <div>
                  <i className="fas fa-flag p-1 text-gray-200 bg-red-900  rounded-lg shadow-md text-xs"></i>
                </div>
              </TooltipTrigger>
              <TooltipContent>Delete Request</TooltipContent>
            </Tooltip>
          </TooltipProvider>
        </div>
      );
    } else {
      return (
        <p className="text-ellipsis text-left whitespace-nowrap overflow-hidden w-32  text-white">
          {data.first_name}
        </p>
      );
    }
  };

  const displayLastName = (data: any) => {
    return (
      <p className="text-ellipsis text-left whitespace-nowrap overflow-hidden w-32  text-white">
        {data.last_name}
      </p>
    );
  };

  // handle modal
  const handleEnbled = (data: any, type: string) => {
    if (!data?.is_approved) {
      setSelectedItem(data);
      setTitle('Customer');
      setPara(
        'Note: By Saving this information customer can perform actionssuch as (login and order).',
      );
      setType(type);
      setIsModal(true);
    } else {
      toast({
        variant: 'success',
        title: `Customer is Already Approved!`,
      });
    }
  };
  // columns

  const columns: ColumnDef<CustomerType>[] = [
    {
      accessorKey: 'First Name',
      header: 'First Name',
      cell: ({ row }) => (
        <div className="capitalize ">
          <TooltipProvider>
            <Tooltip>
              <TooltipTrigger>{displayFirstName(row?.original)}</TooltipTrigger>
              <TooltipContent>
                <p className="text-base font-normal">
                  {row?.original?.first_name}
                </p>
              </TooltipContent>
            </Tooltip>
          </TooltipProvider>
        </div>
      ),
    },
    {
      accessorKey: 'Last Name',
      header: 'Last Name',
      cell: ({ row }) => (
        <div className="capitalize w-28 text-ellipsis whitespace-nowrap overflow-hidden">
          <TooltipProvider>
            <Tooltip>
              <TooltipTrigger>{displayLastName(row?.original)}</TooltipTrigger>
              <TooltipContent>
                <p className="text-base font-normal">
                  {row?.original?.last_name ?? ''}
                </p>
              </TooltipContent>
            </Tooltip>
          </TooltipProvider>
        </div>
      ),
    },
    {
      accessorKey: 'Email',
      header: 'Email',
      cell: ({ row }) => {
        return (
          <div className="flex items-center gap-4 text-ellipsis whitespace-nowrap overflow-hidden">
            <TooltipProvider>
              <Tooltip>
                <TooltipTrigger>
                  {customEmailTruncateHandler(row?.original?.email)}
                </TooltipTrigger>
                <TooltipContent>
                  <p className="text-base font-normal">
                    {row?.original?.email}
                  </p>
                </TooltipContent>
              </Tooltip>
            </TooltipProvider>
          </div>
        );
      },
    },
    {
      accessorKey: 'Phone No',
      header: 'Phone No.',
      cell: ({ row }) => {
        return (
          <div className="flex items-center gap-4 text-ellipsis whitespace-nowrap overflow-hidden">
            <TooltipProvider>
              <Tooltip>
                <TooltipTrigger>
                  {row?.original?.phone_number != null
                    ? row?.original?.phone_number
                    : 'N/A'}
                </TooltipTrigger>
                <TooltipContent>
                  <p className="text-base font-normal">
                    {row?.original?.phone_number != null
                      ? row?.original?.phone_number
                      : 'N/A'}
                  </p>
                </TooltipContent>
              </Tooltip>
            </TooltipProvider>
          </div>
        );
      },
    },
    {
      accessorKey: 'Created at',
      header: 'Created at',
      cell: ({ row }) => (
        <div className="capitalize text-ellipsis whitespace-nowrap overflow-hidden ">
          {displayDate(row?.original?.created_at)}
        </div>
      ),
    },

    {
      id: 'DOB',
      header: 'DOB',

      cell: ({ row }) => {
        return (
          <div className="capitalize text-ellipsis whitespace-nowrap overflow-hidden ">
            {displayDate(row?.original?.dob)}
          </div>
        );
      },
    },

    {
      id: 'Verified Status',
      header: 'Verified Status',

      cell: ({ row }) => {
        return (
          <div className="w-24 text-center">
            <Switch checked={row?.original?.is_verified} disabled={true} />
          </div>
        );
      },
    },
    {
      id: 'Blocked Status',
      header: 'Blocked Status',

      cell: ({ row }) => {
        return (
          <div className="w-24 text-center">
            <Switch checked={row?.original?.is_blocked} disabled={true} />
          </div>
        );
      },
    },

    {
      id: 'actions',
      enableHiding: false,
      header: 'Actions',
      cell: ({ row }) => {
        return (
          <DropdownMenu>
            <DropdownMenuTrigger asChild>
              <Button variant="ghost" className="h-8 w-8 p-0">
                <span className="sr-only">Open menu</span>
                <MoreHorizontal className="h-4 w-4" />
              </Button>
            </DropdownMenuTrigger>
            <DropdownMenuContent align="end">
              <DropdownMenuLabel>Actions</DropdownMenuLabel>
              <DropdownMenuSeparator />

              {row?.original?.is_disabled ? (
                <>
                  <DropdownMenuItem
                    onClick={() => deleteUser(row?.original, 'delete')}
                  >
                    Delete Customer
                  </DropdownMenuItem>
                  <DropdownMenuItem
                    onClick={() => deleteUser(row?.original, 'enable')}
                  >
                    Enable Customer
                  </DropdownMenuItem>
                </>
              ) : (
                <>
                  <DropdownMenuItem
                    onClick={() => deleteUser(row?.original, 'block')}
                  >
                    {row?.original?.is_blocked ? 'Unblock' : 'Block'}
                  </DropdownMenuItem>
                  <DropdownMenuItem
                    onClick={() => deleteUser(row?.original, 'delete')}
                  >
                    Delete
                  </DropdownMenuItem>
                </>
              )}
            </DropdownMenuContent>
          </DropdownMenu>
        );
      },
    },
  ];

  const table = useReactTable({
    data: customerData as CustomerType[],
    columns,
    onSortingChange: setSorting,
    getCoreRowModel: getCoreRowModel(),
    getPaginationRowModel: getPaginationRowModel(),
    getSortedRowModel: getSortedRowModel(),
    getFilteredRowModel: getFilteredRowModel(),
    onColumnVisibilityChange: setColumnVisibility,
    onRowSelectionChange: setRowSelection,
    state: {
      sorting,
      columnVisibility,
      rowSelection,
    },
  });

  function handlePagination(page: number) {
    if (page < 0) return;
    setFilters((prevFilters) => ({ ...prevFilters, first: page }));
  }
  const StatusOptions = [
    {
      name: 'Yes',
      value: true,
    },
    {
      name: 'No',
      value: false,
    },
  ];
  // FILTER OPTIONS
  const roleOptions1 = [
    {
      Icon: 'fal fa-chevron-down',
      text: 'Search',
      filtername: 'searchQuery',
      type: 'text',
    },

    {
      Icon: 'fal fa-chevron-down',
      text: 'Verified',
      filtername: 'is_verified',
      type: 'select',

      filter: StatusOptions,
    },
    {
      Icon: 'fal fa-chevron-down',
      text: 'Delete Request',
      filtername: 'is_disabled',
      type: 'select',

      filter: StatusOptions,
    },
    {
      Icon: 'fal fa-chevron-down',
      text: 'Blocked Users',
      filtername: 'is_blocked',
      type: 'select',

      filter: StatusOptions,
    },
    {
      Icon: 'fal fa-chevron-down',
      text: 'From Date',
      filtername: 'startDate',
      type: 'date',
    },
    {
      Icon: 'fal fa-chevron-down',
      text: 'To Date',
      filtername: 'endDate',
      type: 'date',
    },

    {
      Icon: 'fal fa-chevron-down',
      text: 'Clear Filter',
      filtername: 'Clear',
    },
  ];

  const csvData = [
    [
      'First Name',
      'Last Name',
      'Email',
      'Phone No.',
      'DOB',
      'Verified Status',
      'Created at',
    ],
    ...customerData?.map(
      ({
        first_name,
        last_name,
        email,
        phone_number,
        dob,
        is_verified,
        created_at,
      }) => [
        first_name,
        last_name,
        email,
        phone_number,
        dob?.toLocaleDateString(),
        is_verified ? 'Yes' : 'No',
        created_at?.toLocaleDateString(),
      ],
    ),
  ];

  return (
    <div className="w-full space-y-4">
      <div className="flex items-center justify-between">
        {customerData?.length ? (
          <Button variant="outline">
            <CSVLink filename="customers.csv" data={csvData}>
              Export to CSV
            </CSVLink>
          </Button>
        ) : (
          <div />
        )}

        <div className="flex items-center gap-2">
          <DropdownMenu>
            <DropdownMenuTrigger asChild>
              <Button variant="outline">
                Columns <ChevronDown className="ml-2 h-4 w-4" />
              </Button>
            </DropdownMenuTrigger>
            <DropdownMenuContent align="end">
              {table
                .getAllColumns()
                .filter((column) => column.getCanHide())
                .map((column) => {
                  return (
                    <DropdownMenuCheckboxItem
                      key={column.id}
                      className="capitalize"
                      checked={column.getIsVisible()}
                      onCheckedChange={(value) =>
                        column.toggleVisibility(!!value)
                      }
                    >
                      {column.id}
                    </DropdownMenuCheckboxItem>
                  );
                })}
            </DropdownMenuContent>
          </DropdownMenu>
          <TableFilters
            inputList={roleOptions1}
            item_name={'Customers'}
            value={filterID}
            setValue={setFilterID}
            setFilters={setFilters}
            initial={initialFilters}
          />
        </div>
      </div>

      <div className="rounded-md border border-border">
        <ScrollArea className="w-full">
          <ScrollBar orientation="horizontal"></ScrollBar>
          <Table>
            <TableHeader className="bg-secondary/80">
              {table?.getHeaderGroups().map((headerGroup) => (
                <TableRow key={headerGroup.id}>
                  {headerGroup.headers.map((header) => {
                    return (
                      <TableHead key={header.id}>
                        {header.isPlaceholder
                          ? null
                          : flexRender(
                              header.column.columnDef.header,
                              header.getContext(),
                            )}
                      </TableHead>
                    );
                  })}
                </TableRow>
              ))}
            </TableHeader>
            <TableBody>
              {table?.getRowModel()?.rows?.length ? (
                table?.getRowModel()?.rows?.map((row) => (
                  <TableRow
                    key={row.id}
                    data-state={row.getIsSelected() && 'selected'}
                  >
                    {row.getVisibleCells().map((cell) => (
                      <TableCell key={cell.id}>
                        {flexRender(
                          cell.column.columnDef.cell,
                          cell.getContext(),
                        )}
                      </TableCell>
                    ))}
                  </TableRow>
                ))
              ) : (
                <TableRow>
                  <TableCell
                    colSpan={columns.length}
                    className="h-24 text-center"
                  >
                    No results.
                  </TableCell>
                </TableRow>
              )}
            </TableBody>
          </Table>
        </ScrollArea>
      </div>
      <div className="flex items-center justify-end space-x-2 py-4">
        <div className="flex-1 flex w-[100px] items-center justify-start text-sm font-medium">
          Page {filters.first + 1} of{' '}
          {Math.ceil((data?.count ?? 0) / filters.rows)}
        </div>

        <div className="flex items-center justify-center space-x-6 lg:space-x-8">
          <div className="flex items-center space-x-2">
            <p className="text-sm font-medium">Rows per page</p>
            <Select
              value={`${filters.rows}`}
              onValueChange={(value) => {
                setFilters((prevFilters: any) => ({
                  ...prevFilters,
                  rows: Number(value),
                  first: 0,
                }));
                table.setPageSize(Number(value));
              }}
            >
              <SelectTrigger className="h-8 w-[70px]">
                <SelectValue placeholder={filters.rows} />
              </SelectTrigger>
              <SelectContent side="top">
                {[5, 10, 20, 30, 40, 50].map((pageSize) => (
                  <SelectItem key={pageSize} value={`${pageSize}`}>
                    {pageSize}
                  </SelectItem>
                ))}
              </SelectContent>
            </Select>
          </div>

          <div className="flex items-center space-x-2">
            <Button
              variant="outline"
              className="hidden h-8 w-8 p-0 lg:flex"
              onClick={() => handlePagination(0)}
              disabled={filters.first === 0}
            >
              <span className="sr-only">Go to first page</span>
              <DoubleArrowLeftIcon className="h-4 w-4" />
            </Button>

            <Button
              variant="outline"
              className="h-8 w-8 p-0"
              onClick={() => handlePagination(filters?.first - 1)}
              disabled={filters?.first === 0}
            >
              <span className="sr-only">Go to previous page</span>
              <ChevronLeftIcon className="h-4 w-4" />
            </Button>
            <Button
              variant="outline"
              className="h-8 w-8 p-0"
              onClick={() => handlePagination(filters.first + 1)}
              disabled={
                (filters.first + 1) * filters.rows > (data?.count ?? 0) ||
                Math.ceil((data?.count ?? 0) / filters.rows) ==
                  filters.first + 1
              }
            >
              <span className="sr-only">Go to next page</span>
              <ChevronRightIcon className="h-4 w-4" />
            </Button>

            <Button
              variant="outline"
              className="hidden h-8 w-8 p-0 lg:flex"
              onClick={() =>
                handlePagination(
                  Math.ceil((data?.count ?? 0) / filters.rows) - 1,
                )
              }
              disabled={
                (filters.first + 1) * filters.rows > (data?.count ?? 0) ||
                Math.ceil((data?.count ?? 0) / filters.rows) ==
                  filters.first + 1
              }
            >
              <span className="sr-only">Go to last page</span>
              <DoubleArrowRightIcon className="h-4 w-4" />
            </Button>
          </div>
        </div>
      </div>
      <CustomerDialog
        selectedItem={selectedItem}
        setSelectedItem={setSelectedItem}
        title={title}
        setTitle={setTitle}
        isModal={isModal}
        setIsModal={setIsModal}
        refetch={refetch}
        type={type}
        setType={setType}
        paragraph={para}
      />
      <LoadingDialog open={isLoading} text={'Loading data...'} />
    </div>
  );
}
